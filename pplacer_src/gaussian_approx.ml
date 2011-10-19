(*
Recall
\xi(u) =
\frac{1}{\sqrt{mn}}
[ \sum_i G_i(u) \eta_i - \frac{1}{m+n} (\sum_i G_i(u)) (\sum_i \eta_i) ]

and we want to find

[ \int_T |\xi(u)|^p \, \lambda(du)]^{\frac{1}{p} \wedge 1}.

For convenience, set

\omega(u) = \sum_i G_i(u) \eta_i
\sigma(u) = \sum_i G_i(u)

Note that when going past a placement j of mass \mu_j, \omega(u) gets
incremented by \mu_j \eta_j, and \sigma(u) gets incremented by \mu_j.

For this code, we will be setting up "labeled masses" that correspond to the
indices, then sampling Gaussians, and doing the corresponding summation.
They get stored in labeled_mass_arr.

The quantity (\frac{1}{\sqrt{mn}})^p is called front_coeff. We pull it out of
the integral, but leave it inside the outer exponentiation to avoid having to
deal with the \frac{1}{p} \wedge 1 thing again.
*)

open Ppatteries
open Mass_map

(* this exception shows up if the average mass isn't one *)
exception Avg_mass_not_one of float

(* These are masses which are associated with an index.
 * The index is so that we can sample the eta's, then multiply the eta_j with
 * the appropriate mu_j. *)
type labeled_mass = { pquery_num: int ;
                      mass: float }

(* the intermediate things held for calculation; see scan *)
type calc_intermediate = { omega: float ref;   (* weighted sum of gaussians *)
                           sigma: float ref; } (* sum of weights *)

let get_omega i = !(i.omega)
let get_sigma i = !(i.sigma)

let intermediate_sum i1 i2 =
  { omega = ref ((get_omega i1) +. (get_omega i2));
    sigma = ref ((get_sigma i1) +. (get_sigma i2)) }

(* This looks a bit nuts, and it kind of is, but we are actually only doing this
 * operation O(#edges of the tree) times. For bifurcating trees these are lists
 * of two, and the above is actually the most efficient way to go. *)
let intermediate_list_sum = List.reduce intermediate_sum

(* Note: upre1 and upre2 must have unit mass per placement. *)
let pair_approx ?(normalization=1.) rng n_samples p t upre1 upre2 =
  let np1 = List.length upre1
  and np2 = List.length upre2
  and int_inv x = 1. /. (float_of_int x)
  in
  let labeled_mass_arr = Array.make (1+Gtree.top_id t) []
  and pquery_counter = ref 0
  and front_coeff = (sqrt(int_inv(np1 * np2))) ** p
  (* sample is a (mutable) array with all of the Gaussian samples (the etas) *)
  and sample = Array.make (np1 + np2) 0.
  in
  (* Initialize the labeled_mass_arr array with mass from both samples. *)
  List.iter
    (List.iter
    (* recall multimul = mass unit list with multiplicity *)
      (fun {Pre.multi; Pre.mul} ->
        if multi <> 1. then
          failwith
            "Nontrivial transformed multiplicity for Gaussian significance not \
            supported at the moment.";
        List.iter
          (fun mu ->
            labeled_mass_arr.(mu.Pre.loc) <-
              (mu.Pre.distal_bl,
              { pquery_num = !pquery_counter;
              mass = multi *. mu.Pre.mass })
                :: (labeled_mass_arr.(mu.Pre.loc)))
          mul;
        incr pquery_counter))
    [upre1; upre2];
  (* Sort by position along the edge. *)
  Array.modify (List.sort (comparing fst)) labeled_mass_arr;
  (* The sampling routine. *)
  let sample_gaussians () =
   Array.iteri
     (fun i _ -> sample.(i) <- Gsl_randist.gaussian rng ~sigma:1.)
     sample
  in
  (* Go "past" a labeled mass. *)
  let update_data data lm =
    data.omega := (get_omega data) +. lm.mass *. sample.(lm.pquery_num);
    data.sigma := (get_sigma data) +. lm.mass;
  in
  (* Take the samples and do the calculation. *)
  List.init
    n_samples
    (fun _ ->
      sample_gaussians ();
      (* sample_avg is \frac{1}{m+n} (\sum_i \eta_i) *)
      let sample_avg =
        (int_inv (np1 + np2)) *. (Array.fold_left (+.) 0. sample) in
      (* xi is
       * \sum_i G_i(u) \eta_i - \frac{1}{m+n} (\sum_i G_i(u)) (\sum_i \eta_i)
       * \omega(u) - \sigma(u) \frac{1}{m+n} (\sum_i \eta_i) *)
      let to_xi_p_times_bl p data bl =
        ((abs_float ((get_omega data) -. (get_sigma data) *. sample_avg)) ** p) *. bl in
      (* The total for a single edge. *)
      let edge_total id =
        Kr_distance.total_along_edge
          (to_xi_p_times_bl p)
          (Gtree.get_bl t id)
          labeled_mass_arr.(id)
          update_data
      (* Make sure that the average weight totals to one. *)
      and check_final_data data =
        let avg_weight = int_inv (np1 + np2) *. (get_sigma data) in
        if abs_float (avg_weight -. 1.) > Kr_distance.tol then
          raise (Avg_mass_not_one (avg_weight-.1.))
      in
      (* as described up top, front_coeff goes outside the integral but inside
       * the outer exponentiation. *)
      (front_coeff *.
        (Kr_distance.total_over_tree
          edge_total
          check_final_data
          intermediate_list_sum
          (fun () -> { omega = ref 0.; sigma = ref 0.; })
          (Gtree.get_stree t))
        /. normalization)
        ** (Kr_distance.outer_exponent p))
