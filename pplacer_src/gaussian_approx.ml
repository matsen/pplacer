(*
 * see scan: normal_approx.pdf

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

*)

open MapsSets
open Fam_batteries
open Mass_map

(* for the time being, we interpret multiplicity literally *)
let transform = Mass_map.no_transform

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
let intermediate_list_sum = ListFuns.complete_fold_left intermediate_sum

(* recall that transform is globally set up top for the time being *)
let pair_approx ?(normalization=1.) rng n_samples p t pre1 pre2 =
  (* make sure that the pres have unit mass per placement *)
  let upre1 = Mass_map.Pre.unitize_mass transform pre1
  and upre2 = Mass_map.Pre.unitize_mass transform pre2
  in
  let np1 = List.length upre1
  and np2 = List.length upre2
  and int_inv x = 1. /. (float_of_int x)
  in
  let labeled_mass_arr = Array.make (1+Gtree.top_id t) []
  and pquery_counter = ref 0
  and front_coeff = sqrt(int_inv(np1 * np2))
  (* sample is a (mutable) array with all of the Gaussian samples (the etas) *)
  and sample = Array.make (np1 + np2) 0.
  in
  (* Initialize the labeled_mass_arr array with mass from both samples. *)
  List.iter
    (List.iter
      (fun multimul ->
        let trans_multi = transform multimul.Pre.multi in
        if trans_multi <> 1. then
          failwith
            "Nontrivial multiplicity for Gaussian significance not \
            supported at the moment.";
        List.iter
          (fun mu ->
            labeled_mass_arr.(mu.Pre.loc) <-
              (mu.Pre.distal_bl,
              { pquery_num = !pquery_counter;
              mass = trans_multi *. mu.Pre.mass })
              :: (labeled_mass_arr.(mu.Pre.loc)))
          multimul.Pre.mul;
        incr pquery_counter))
    [upre1; upre2];
  (* Sort by position along the edge. *)
  for i=0 to (Array.length labeled_mass_arr) - 1 do
    labeled_mass_arr.(i) <-
      List.sort
        (fun (a1,_) (a2,_) -> compare a1 a2)
        labeled_mass_arr.(i)
  done;
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
  ListFuns.init
    n_samples
    (fun _ ->
      sample_gaussians ();
      (* sample_avg is \frac{1}{m+n} (\sum_i \eta_i) *)
      let sample_avg =
        (int_inv (Array.length sample)) *.
          (Array.fold_left (+.) 0. sample) in
      (* xi is
       * \sum_i G_i(u) \eta_i - \frac{1}{m+n} (\sum_i G_i(u)) (\sum_i \eta_i)
       * \omega(u) - \sigma(u) \frac{1}{m+n} (\sum_i \eta_i) *)
      let to_xi_p p data =
        (abs_float ((get_omega data) -. (get_sigma data) *. sample_avg)) ** p in
      (* The total for a single edge. *)
      let edge_total id =
        Kr_distance.total_along_edge
          (to_xi_p p)
          (Gtree.get_bl t id)
          labeled_mass_arr.(id)
          update_data
      (* Make sure that the average weight totals to one. *)
      and check_final_data data =
        let avg_weight = int_inv (np1 + np2) *. (get_sigma data) in
        if abs_float (avg_weight -. 1.) > Kr_distance.tol then
          raise (Avg_mass_not_one (avg_weight-.1.))
      in
      front_coeff *.
        ((Kr_distance.total_over_tree
          edge_total
          check_final_data
          intermediate_list_sum
          (fun () -> { omega = ref 0.; sigma = ref 0.; })
          t) /. normalization)
        ** (Kr_distance.outer_exponent p))
