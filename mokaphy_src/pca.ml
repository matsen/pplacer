(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries

let tolerance = 1e-3

(* *** general PCA stuff *** *)

let dot = ArrayFuns.fold_left2 (fun s x1 x2 -> s +. (x1 *. x2)) 0. 

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
 * Assuming rectangularity, etc, and clearly not highly optimized.
 * *)
let covariance_matrix ?scale faa = 
  let n = Array.length faa.(0) in 
  let m = Gsl_matrix.create n n 
  and col = Array.init n (column faa)
  in
  let base_cov i j = Gsl_stats.covariance col.(i) col.(j) in
  let f = match scale with
    | None | Some false -> base_cov
    | Some true -> 
        let dia = Array.init n (fun i -> sqrt(base_cov i i)) in
        (fun i j -> 
          let num = base_cov i j
          and denom = dia.(i) *. dia.(j) 
          in
          if denom = 0. then (assert (num = 0.); 0.)
          else num /. denom)
  in
  for i=0 to n-1 do
    for j=i to n-1 do
      let cov = f i j in
      m.{i,j} <- cov;
      m.{j,i} <- cov;
    done;
  done;
  m

(* make an array of (eval, evect) tuples. Keep only the top n_keep. *)
let gen_pca ?n_keep ?scale faa = 
  let (vals, vects) as system = my_symmv (covariance_matrix ?scale faa) in
  match n_keep with
  | None -> system
  | Some n -> (Array.sub vals 0 n, Array.sub vects 0 n)


(* *** splitify *** *)

let splitify x = x -. (1. -. x)

let soft_find i m = if IntMap.mem i m then IntMap.find i m else 0.

let arr_of_map len m = Array.init len (fun i -> soft_find i m)

let map_of_arr a = 
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

let map_filter f m = 
  IntMap.fold
    (fun k v m -> if f k v then IntMap.add k v m else m)
    m
    IntMap.empty

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
  assert(abs_float(1. -. total) < tolerance);
  !m

(* Take a placerun and turn it into a vector which is indexed by the edges of
 * the tree.
 * Later we may cut the edge mass in half; right now we don't do anything with it. *)
let splitify_placerun transform weighting criterion pr = 
  let preim = Mass_map.Pre.of_placerun weighting criterion pr
  and t = Placerun.get_ref_tree pr
  in 
  arr_of_map 
    (1+(Gtree.top_id t))
    (IntMap.map 
      splitify 
      (below_mass_map (Mass_map.By_edge.of_pre transform preim) t))

let heat_map_of_floatim multiplier m = 
  let min_width = 1. in 
  IntMap.map
    (fun v ->
      if v = 0. then []
      else begin
        let width = multiplier *. (abs_float v) in
        (Heat_tree.simple_color_of_heat v)::
          (if width < min_width then []
           else [Decor.width width])
      end)
    m

let heat_tree_of_floatim multiplier t m =
  Placeviz_core.spread_short_fat 1e-2
    (Decor_gtree.add_decor_by_map t ((heat_map_of_floatim multiplier) m))

let save_named_fal fname nvl = 
  Csv.save 
    fname
    (List.map
      (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
      nvl)

let pca_complete ?scale transform
      weighting criterion multiplier write_n refpkgo out_prefix prl = 
  let prt = Cmds_common.list_get_same_tree prl in
  let t = match refpkgo with 
  | None -> Decor_gtree.of_newick_gtree prt
  | Some rp -> 
      Cmds_common.check_refpkgo_tree prt refpkgo;
      Refpkg.get_tax_ref_tree rp
  in
  let data = List.map (splitify_placerun transform weighting criterion) prl
  in
  let (eval, evect) = gen_pca ?scale ~n_keep:write_n (Array.of_list data)
  in
  let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
  and names = (List.map Placerun.get_name prl)
  in
  Phyloxml.named_tree_list_to_file
    (List.map
      (fun (eval, evect) ->
        (Some (string_of_float eval),
        heat_tree_of_floatim multiplier t (map_of_arr evect)))
      combol)
    (out_prefix^".xml");
  save_named_fal
    (out_prefix^".rot")
    (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
  save_named_fal
    (out_prefix^".trans")
    (List.combine 
      names 
      (List.map (fun d -> Array.map (dot d) evect) data));
  save_named_fal
    (out_prefix^".edgediff")
    (List.combine names data);
  ()
