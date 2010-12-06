(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

(* *** general PCA stuff *** *)

(* returns array of values, and then array of vectors (i.e. left eigenmatrix if
  * considered as a matrix) *)
let my_symmv m = 
  let (evalv, evectm) = Gsl_eigen.symmv (`M (m)) in
  Gsl_eigen.symmv_sort (evalv, evectm) Gsl_eigen.VAL_DESC;
  (* GSL makes nice column vectors *)
  Gsl_matrix.transpose_in_place evectm;
  (Gsl_vector.to_array evalv, Gsl_matrix.to_arrays evectm)

let column aa j = Array.init (Array.length aa) (fun i -> aa.(i).(j))

(* pass in an a by n array of arrays, and make the corresponding n by n
 * covariance matrix. this is the standard way, such that rows represent
 * observations and columns represent variables. 
 * Assuming rectangularity, etc. 
 * *)
let covariance_matrix faa = 
  let n = Array.length faa.(0) in 
  let m = Gsl_matrix.create n n in
  let col = Array.init n (column faa) in
  for i=0 to n-1 do
    for j=i to n-1 do
      let cov = Gsl_stats.covariance col.(i) col.(j) in
      m.{i,j} <- cov;
      m.{j,i} <- cov;
    done;
  done;
  m

(* make an array of (eval, evect) tuples *)
let gen_pca faa = 
  my_symmv (covariance_matrix faa)


(* *** splitify *** *)

(* abs(x - (1-x)) *)
let splitify x = abs_float (1. -. 2. *. x)

let soft_find i m = if IntMap.mem i m then IntMap.find i m else 0.

(* get the mass below the given edge, excluding that edge *)
let below_mass_map edgem t = 
  let m = ref IntMap.empty in
  let total = 
    Gtree.recur
      (fun i below_massl -> 
        let below_tot = List.fold_left ( +. ) 0. below_massl in
        m := IntMapFuns.check_add i below_tot (!m);
        (soft_find i edgem) +. below_tot)
      (fun i -> soft_find i edgem)
      t
  in 
  assert(abs_float(1. -. total) < 1e-3);
  !m

let arr_of_map len m = 
  Array.init len (fun i -> soft_find i m)

let map_of_arr a = 
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

(* later we may cut the edge mass in half *)
let splitify_placerun weighting criterion pr = 
  let preim = Mass_map.Pre.of_placerun weighting criterion pr
  and t = Placerun.get_ref_tree pr
  in 
  arr_of_map 
    (1+(Gtree.top_id t))
    (IntMap.map 
      splitify 
      (below_mass_map (Mass_map.By_edge.of_pre preim) t))

let map_filter f m = 
  IntMap.fold
    (fun k v m -> if f k v then IntMap.add k v m else m)
    m
    IntMap.empty

let heat_map_of_floatim m = 
  let multiplier = 50. 
  and min_width = 1.
  in 
  IntMap.map
    (fun v ->
      if v = 0. then []
      else begin
        let width = multiplier *. v in
        (Heat_tree.simple_color_of_heat v)::
          (if width < min_width then []
           else [Decor.width width])
      end)
    m

let heat_tree_of_floatim t m =
  Placeviz_core.spread_short_fat 1e-2
    (Decor_gtree.add_decor_by_map t (heat_map_of_floatim m))

let pca_complete weighting criterion write_n out_fname prl = 
  let t = Decor_gtree.of_newick_gtree (Cmds_common.list_get_same_tree prl)
  and (eval, evect) = 
    gen_pca 
      (Array.of_list 
        (List.map (splitify_placerun weighting criterion) prl))
  in
  let to_write = 
    Base.list_sub ~len:write_n 
      (List.combine (Array.to_list eval) (Array.to_list evect))
  in
  Phyloxml.named_tree_list_to_file
    (List.map
      (fun (eval, evect) ->
        (Some (string_of_float eval),
        heat_tree_of_floatim t (map_of_arr evect)))
      to_write)
    out_fname

let () = 
  pca_complete Mass_map.Weighted Placement.ml_ratio 5 "test_pca.xml" prl

    (*
let prl = 
  (List.map Placerun_io.of_file 
[
  "/home/bvdiversity/working/matsen/interesting/p1z1r2/p1z1r2.place";
  "/home/bvdiversity/working/matsen/interesting/p1z1r34/p1z1r34.place";
  "/home/bvdiversity/working/matsen/interesting/p1z2r19/p1z2r19.place";
  "/home/bvdiversity/working/matsen/interesting/p4z1r2/p4z1r2.place";
  "/home/bvdiversity/working/matsen/interesting/p4z2r22/p4z2r22.place";
])

let out = pca Mass_map.Weighted Placement.ml_ratio prl

*)

(*
a <- c(9,3,5)
b <- c(3,4,1)
m <- cbind(a, b)
#     a b
# [1,] 9 3
# [2,] 3 4
# [3,] 5 1

cov(m)
#           a          b
# a  9.3333333 -0.6666667
# b -0.6666667  2.3333333
*)

let build_faa intll = 
  Array.of_list 
    (List.map 
      (fun l -> Array.of_list (List.map float_of_int l))
      intll)

let x = 
  build_faa 
  [
    [9; 3];
    [3; 4];
    [5; 1];
  ]

let cov = covariance_matrix x

let m = 
  Gsl_matrix.of_arrays 
    [|
      [|1.; 2.|]; 
      [|2.; -2.|];
    |]
let true_v = 
  Gsl_matrix.of_arrays
    [|
      [| 2.; -1. |];
      [| 1.;  2.  |];
    |]

let check = Fam_gsl_matvec.allocMatMatMul m true_v

let evecta = my_symmv m


