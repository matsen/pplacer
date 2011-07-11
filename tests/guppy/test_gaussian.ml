open OUnit
open Test_util

let transform = Mass_map.no_transform
let rng = make_rng 1
let n_samples = 1000
let criterion = Placement.ml_ratio
let weighting = Mass_map.Weighted
let p = 1.

(* Just a little regression test. *)
let test_gaussian correct fname1 fname2 =
  let pr1 = Placerun_io.of_any_file (tests_dir^fname1)
  and pr2 = Placerun_io.of_any_file (tests_dir^fname2)
  in
  let t = Placerun.get_same_tree pr1 pr2 in
  let prepare pr =
    Mass_map.Pre.unitize_mass transform
      (Mass_map.Pre.of_placerun weighting criterion pr)
  in
  let f pr1 pr2 =
    let null_dists =
      Gaussian_approx.pair_approx transform rng n_samples p t pr1 pr2 in
    let original_dist =
      Kr_distance.scaled_dist_of_pres transform p t pr1 pr2 in
    Guppy_kr.list_onesided_pvalue null_dists original_dist
  in
  (Printf.sprintf "%f <> gaussian %s %s" correct pr1.Placerun.name pr2.Placerun.name) @?
  (correct = f (prepare pr1) (prepare pr2))

let suite = [
  "gaussian" >::: [
    "coastal.v.upwelling" >::
      (fun _ ->
         test_gaussian 0.715 "data/psbA/coastal.jplace" "data/psbA/upwelling.jplace");
  ]
]

