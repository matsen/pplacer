(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * here we make the dp GLV int maps using Likestree.
 *)

open MapsSets
open Fam_batteries

let ppr_glv_intmap = IntMapFuns.ppr_gen Glv.ppr_glv


(* *** making them *** *)

(* convert from a list (rates) of arrays (sites) of likelihood vector maps
 * (locations). that's what Likestree makes.
 *)
let of_lv_map_arr_arr locs lv_map_arr_arr = 
  assert(lv_map_arr_arr <> [||]);
  let n_rates = Array.length lv_map_arr_arr
  and n_sites = Array.length (lv_map_arr_arr.(0)) 
  in
  try
    List.fold_right 
      (fun loc ->
        let g = Glv.make_empty ~n_sites ~n_rates in
        Array.iteri
          (fun rate ->
            (Array.iteri
              (fun site m ->
                Glv.set g ~site ~rate (IntMap.find loc m))))
          lv_map_arr_arr;
        IntMap.add loc g)
      locs
      IntMap.empty
  with
  | Not_found -> assert(false)
            

(* dp_of_data:
 *)
let dp_of_data model align gtree locs = 
  let dp_arr = 
    Array.map
      (fun rate ->
        Likestree.distoproximal_of_aln_and_gtree 
          (Model.seq_type model) (Model.diagdq model) align gtree rate)
      (Model.rates model) in
  let result = 
  (of_lv_map_arr_arr locs (Array.map fst dp_arr),
   of_lv_map_arr_arr locs (Array.map snd dp_arr)) in
  result

let mask mask_arr m = IntMap.map (Glv.mask mask_arr) m

(*
let it = Stree_io.of_newick_str "((x:0.2,y:3e-2):0.05,z:1e-5):0."
let mini = [| ("x","AA"); ("y","AT"); ("z","AA"); |]
let model = Model.build "GTR" false "JC.stats.txt" mini [|1.; 2.|]
let (x,y) = dp_of_data model Alignment.Nucleotide_seq mini it [0;1;2;3;4]
let mk = mask [|false; true|] x
*)
