(* see scan: normal_approx.pdf *)

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
type calc_intermediate = { omega: float ref;   (* weighted sum of normals *)
                           sigma: float ref; } (* sum of weights *)

(* getters to avoid references *)
let get_omega i = !(i.omega)
let get_sigma i = !(i.sigma)

let intermediate_sum i1 i2 =
  { omega = ref ((get_omega i1) +. (get_omega i2));
    sigma = ref ((get_sigma i1) +. (get_sigma i2)) }

(* this seems like a pretty crazy way to do things.
 * I should use the mutables in the calc_intermediate. *)
let intermediate_list_sum =
  ListFuns.complete_fold_left intermediate_sum

(* recall that transform is globally set up top for the time being *)
let normal_pair_approx ?(normalization=1.) rng n_samples p t pre1 pre2 =
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
  and sample = Array.make (np1 + np2) 0.
  in
  (* initialize the labeled_mass_arr array *)
  List.iter
    (List.iter
      (fun multimul ->
        let trans_multi = transform multimul.Pre.multi in
        assert(trans_multi = 1.); (* for debugging *)
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
  (* sort along the edge *)
  for i=0 to (Array.length labeled_mass_arr) - 1 do
    labeled_mass_arr.(i) <-
      List.sort
        (fun (a1,_) (a2,_) -> compare a1 a2)
        labeled_mass_arr.(i)
  done;
  (* the sampling routine *)
  let sample_normals () =
   Array.iteri
     (fun i _ -> sample.(i) <- Gsl_randist.gaussian rng ~sigma:1.)
     sample
  in
  (* go "past" a labeled mass *)
  let update_data data lm =
    data.omega := (get_omega data) +. lm.mass *. sample.(lm.pquery_num);
    data.sigma := (get_sigma data) +. lm.mass;
  in
  (* take the samples and do the calculation *)
  ListFuns.init
    n_samples
    (fun _ ->
      sample_normals ();
      let sample_avg =
        (int_inv (Array.length sample)) *.
          (Array.fold_left (+.) 0. sample) in
      let to_xi_p p data =
        (abs_float ((get_omega data) -. (get_sigma data) *. sample_avg)) ** p in
      (* total across all of the edges of the tree *)
      let edge_total id =
        Kr_distance.total_along_edge
          (to_xi_p p)
          (Gtree.get_bl t id)
          labeled_mass_arr.(id)
          update_data
      (* make sure that the average weight totals to one *)
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
