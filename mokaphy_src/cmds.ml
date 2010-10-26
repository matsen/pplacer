(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 *)

open MapsSets
open Fam_batteries

(* *** common *** *)


(* for out_fname options *)
let ch_of_fname = function
  | "" -> stdout
  | s -> open_out s

let wrap_output fname f = 
  let ch = ch_of_fname fname in
  f ch;
  if ch <> stdout then close_out ch

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

let pre_of_pr ~is_weighted ~use_pp pr = 
  Mass_map.Pre.of_placerun 
    (Mokaphy_prefs.weighting_of_bool is_weighted)
    (Mokaphy_prefs.criterion_of_bool use_pp)
    pr

let prel_of_prl ~is_weighted ~use_pp prl = 
  List.map (pre_of_pr ~is_weighted ~use_pp) prl

  
(* *** output tools *** *)
(* there is a lack of parallelism here, as write_unary takes placeruns, while
 * uptri takes an uptri, but uptri needs to be more general. *)

let write_unary pr_to_float prl ch = 
  String_matrix.write_padded
   ch
   (Array.map
     (fun pr ->
       [| 
         Placerun.get_name pr; 
         Printf.sprintf "%g" (pr_to_float pr);
       |])
   (Array.of_list prl))


let write_uptri fun_name list_output namea u ch = 
  if Uptri.get_dim u = 0 then 
    failwith(Printf.sprintf "can't do %s with fewer than two place files" fun_name);
  if list_output then begin
    String_matrix.write_padded ch
      (Array.of_list
        ([|"sample_1"; "sample_2"; fun_name;|]::
          (let m = ref [] in
          Uptri.iterij
            (fun i j s -> m := [|namea.(i); namea.(j); s|]::!m)
            (Uptri.map (Printf.sprintf "%g") u);
          List.rev !m)))
  end
  else begin
    Printf.fprintf ch "%s distances:\n" fun_name;
    Mokaphy_base.write_named_float_uptri ch namea u;
  end


(* *** BARY BARY BARY BARY BARY *** *)
let make_bary_tree t prel =
  let bary_map = 
    IntMapFuns.of_pairlist_listly
      (ListFuns.mapi
        (fun i pre ->
          let (loc, pos) = Barycenter.of_pre t pre in
          (loc,
            (pos, 
            Gtree.Internal_node,
            (fun bl -> 
              new Decor_bark.decor_bark 
                (`Of_bl_name_boot_dlist 
                   (Some bl, None, None, [Decor.dot i]))))))
        prel)
  in
  Gtree.add_subtrees_by_map (Decor_gtree.of_newick_gtree t) bary_map

let bary prefs prl = 
  let t = list_get_same_tree prl in
  let prel = 
    prel_of_prl 
      ~is_weighted:(Mokaphy_prefs.Bary.weighted prefs)
      ~use_pp:(Mokaphy_prefs.Bary.use_pp prefs)
      prl
  in
  if prl <> [] then begin
    let fname = match Mokaphy_prefs.Bary.out_fname prefs with
      | "" -> (cat_names prl)^".bary.xml"
      | s -> s
    in
    Phyloxml.named_tree_to_file
      (chop_suffix_if_present fname ".xml") (* tree name *)
      (make_bary_tree t prel)
      fname
  end



(* *** HEAT HEAT HEAT HEAT HEAT *** *)
let heat prefs = function
  | [pr1; pr2] as prl ->
      let fname = match Mokaphy_prefs.Heat.out_fname prefs with
        | "" -> (cat_names prl)^".heat.xml"
        | s -> s
      in
      let my_pre_of_pr = 
        pre_of_pr
          ~is_weighted:(Mokaphy_prefs.Heat.weighted prefs)
          ~use_pp:(Mokaphy_prefs.Heat.use_pp prefs)
      in
      Phyloxml.named_tree_to_file
        (chop_suffix_if_present fname ".xml") (* tree name *)
        (Heat_tree.make_heat_tree prefs 
          (Placerun.get_same_tree pr1 pr2)
          (my_pre_of_pr pr1) (my_pre_of_pr pr2))
        fname
  | [] -> () (* e.g. heat -help *)
  | _ -> failwith "Please specify exactly two place files to make a heat tree."



(* *** KR KR KR KR KR *** *)
let kr prefs prl = 
  wrap_output (Mokaphy_prefs.KR.out_fname prefs)
    (fun ch -> Kr_core.core ch prefs prl)

(* *** PD PD PD PD PD *** *)
let pd prefs prl = 
  let pd_cmd = 
    if Mokaphy_prefs.PD.normalized prefs then Pd.normalized_of_pr
    else Pd.of_pr
  in
  wrap_output 
    (Mokaphy_prefs.PD.out_fname prefs) 
    (write_unary 
      (pd_cmd (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PD.use_pp prefs)))
      prl)


(* *** PDFRAC PDFRAC PDFRAC PDFRAC PDFRAC *** *)
let pdfrac prefs prl = 
  let t = list_get_same_tree prl
  and pra = Array.of_list prl
  in
  let inda = 
    Array.map
      (Induced.of_placerun 
        (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PDFrac.use_pp prefs)))
      pra
  in
  wrap_output 
    (Mokaphy_prefs.PDFrac.out_fname prefs) 
    (write_uptri
      "pdfrac"
      (Mokaphy_prefs.PDFrac.list_output prefs)
      (Array.map Placerun.get_name pra)
      (Uptri.init
        (Array.length inda)
        (fun i j -> Pdfrac.of_induceds t inda.(i) inda.(j))))



(* *** AVGDIST AVGDIST AVGDIST AVGDIST AVGDIST  *** *)

let make_dist_fun prefs prl = 
  Pquery_distances.dist_fun_of_w 
    (Mokaphy_prefs.weighting_of_bool (Mokaphy_prefs.Avgdst.weighted prefs))
    (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.Avgdst.use_pp prefs))
    (Edge_rdist.build_ca_info (list_get_same_tree prl))

let uavgdst prefs prl = 
  wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (write_unary
      (Avgdst.of_placerun 
        (make_dist_fun prefs prl)
        (Mokaphy_prefs.Avgdst.exponent prefs))
      prl)

let bavgdst prefs prl = 
  let pra = Array.of_list prl in
  wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (write_uptri
      "bavgdst"
      (Mokaphy_prefs.Avgdst.list_output prefs)
      (Array.map Placerun.get_name pra)
      (Uptri.init
        (Array.length pra)
        (fun i j -> 
          Avgdst.of_placerun_pair 
            (make_dist_fun prefs prl)
            (Mokaphy_prefs.Avgdst.exponent prefs)
            pra.(i) 
            pra.(j))))
