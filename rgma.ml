(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(* rgma.ml
 * rgma stands for rate-glv map array, that is an array (over sites) of maps
 * (over locations) of glvs which have been combined across rates.
 * this is a convenient thing to work with because we can mask then combine
 * across the masked sites to get our map from locations to masked glv's.
 *)
open MapsSets
open Fam_batteries

let ppr_glv_intmap = IntMapFuns.ppr_gen Glv.ppr_glv
let ppr_rgma = Ppr.ppr_array ppr_glv_intmap


(* *** useful functions *** *)
(* combine_maps:
 * combine all of the maps in a given list together using combine_fun.
 * the desired keys are in keys.
 *)
let combine_intmaps combine_fun keys maps = 
  Base.complete_fold_left
    (Base.combine_over_intmaps combine_fun keys)
    maps


(* *** making rgmas *** *)

(* convert from a list (rates) of arrays (sites) of likelihood vector maps
 * (locations). that's what Likestree makes.
 *)
let of_lv_map_arr_list locs lv_map_arr_list = 
  Base.complete_fold_left
    (ArrayFuns.map2 
      (Base.combine_over_intmaps 
        Glv.combine_over_rates 
        locs))
  (List.map (* convert lvs to glvs *)
    (Array.map (IntMap.map Glv.of_like_vect)) 
    lv_map_arr_list)

(* of_data:
 *)
let dp_rgma_of_data model seq_type align istree locs = 
  print_endline "running dp"; flush_all ();
  let dp_list = 
    List.map
      (fun rate ->
        Likestree.distoproximal_of_aln_and_istree 
          seq_type (Model.diagdq model) align istree rate)
      (Model.rates model) in
  print_endline "ending dp"; flush_all ();
  (of_lv_map_arr_list locs (List.map fst dp_list),
   of_lv_map_arr_list locs (List.map snd dp_list))


(* *** turning rgmas into glv_maps *** *)

(* to_glv_map:
 * combine to make a map from locations to glvs with sites and rates.
 *)
let to_glv_map locs rgma = 
  combine_intmaps Glv.combine_over_sites locs (Array.to_list rgma)


(* to_glv_map_masked:
 * as above but with masking
 *)
let to_glv_map_masked mask_arr locs rgma = 
  combine_intmaps 
    Glv.combine_over_sites locs (Base.mask_to_list mask_arr rgma)

(*
let it = Stree_io.of_newick_str "((x:0.2,y:3e-2):0.05,z:1e-5):0."
let mini = [| ("x","AA"); ("y","AT"); ("z","AA"); |]
let model = Model.build "GTR" false "JC.stats.txt" mini [1.; 2.]
let (x,y) = dp_rgma_of_data model Alignment.Nucleotide_seq mini it [0;1;2;3;4]
*)
