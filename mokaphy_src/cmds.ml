(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 * le menu:
   * bary
   *
 *)

open MapsSets
open Fam_batteries

(* *** common *** *)

(* weighted option *)
let weighting_of_bool = function
  | true -> Mass_map.Weighted 
  | false -> Mass_map.Unweighted 

(* use_pp option *)
let criterion_of_bool = function
  | true -> Placement.post_prob
  | false -> Placement.ml_ratio

(* for out_fname options *)
let ch_of_fname = function
  | "" -> stdout
  | s -> open_out s

let chop_suffix_if_present s suff = 
  if Filename.check_suffix s suff then Filename.chop_suffix s suff
  else s

(* make sure all the trees in the placerun list are the same *)
let list_get_same_tree = function
  | [] -> assert(false)
  | [x] -> Placerun.get_ref_tree x
  | hd::tl -> List.hd (List.map (Placerun.get_same_tree hd) tl)

let cat_names prl = 
  String.concat "." (List.map Placerun.get_name prl)


(* *** BARY BARY BARY BARY BARY *** *)
let make_bary_tree weighting criterion prl =
  let bary_map = 
    IntMapFuns.of_pairlist_listly
      (ListFuns.mapi
        (fun i pr ->
          let (loc, pos) = 
            Barycenter.of_placerun weighting criterion pr in
          (loc,
            (pos, 
            Gtree.Internal_node,
            (fun bl -> 
              new Decor_bark.decor_bark 
                (`Of_bl_name_boot_dlist 
                   (Some bl, None, None, [Decor.dot i]))))))
        prl)
  in
  (* we don't use get_same_tree here because it's a whole array *)
  Gtree.add_subtrees_by_map
    (Decor_gtree.of_newick_gtree (list_get_same_tree prl))
    bary_map

let bary prefs prl = 
  if prl <> [] then begin
    let fname = match Mokaphy_prefs.Bary.out_fname prefs with
      | "" -> (cat_names prl)^".bary.xml"
      | s -> s
    in
    Phyloxml.named_tree_to_file
      (chop_suffix_if_present fname ".xml") (* tree name *)
      (make_bary_tree 
        (weighting_of_bool (Mokaphy_prefs.Bary.weighted prefs))
        (criterion_of_bool (Mokaphy_prefs.Bary.use_pp prefs))
        prl)
      fname
  end



(* *** HEAT HEAT HEAT HEAT HEAT *** *)
let heat prefs = function
  | [pr1; pr2] as prl ->
      let fname = match Mokaphy_prefs.Heat.out_fname prefs with
      | "" -> (cat_names prl)^".heat.xml"
      | s -> s
      in
    Phyloxml.named_tree_to_file
      (chop_suffix_if_present fname ".xml") (* tree name *)
      (Heat_tree.make_heat_tree 
        prefs
        (weighting_of_bool (Mokaphy_prefs.Heat.weighted prefs))
        (criterion_of_bool (Mokaphy_prefs.Heat.use_pp prefs))
        pr1 pr2)
      fname
  | [] -> () (* e.g. heat -help *)
  | _ -> failwith "Please specify exactly two place files to make a heat tree."



(* *** KR KR KR KR KR *** *)
let kr prefs prl = 
  let ch = ch_of_fname (Mokaphy_prefs.KR.out_fname prefs) in
  Kr_core.core 
    ch 
    prefs 
    (criterion_of_bool (Mokaphy_prefs.KR.use_pp prefs))
    (Array.of_list prl);
  if ch <> stdout then close_out ch



(* *** PD PD PD PD PD *** *)
let pd prefs prl = 
  let ch = ch_of_fname (Mokaphy_prefs.PD.out_fname prefs) in
  String_matrix.write_padded
    ch
    (Array.map
      (fun pr ->
        [| 
        Placerun.get_name pr; 
        Printf.sprintf 
          "%g" 
          (Pd.pd_of_pr
            (criterion_of_bool (Mokaphy_prefs.PD.use_pp prefs))
            pr);
        |])
    (Array.of_list prl));
  if ch <> stdout then close_out ch



(* *** PDFRAC PDFRAC PDFRAC PDFRAC PDFRAC *** *)

let pdfrac prefs prl = 
  let ch = ch_of_fname (Mokaphy_prefs.PDFrac.out_fname prefs) in
  String_matrix.write_padded
    ch
    (Array.map
      (fun pr ->
        [| 
        Placerun.get_name pr; 
        Printf.sprintf 
          "%g" 
          (Pd.pd_of_pr
            (criterion_of_bool (Mokaphy_prefs.PDFrac.use_pp prefs))
            pr);
        |])
    (Array.of_list prl));
  if ch <> stdout then close_out ch
