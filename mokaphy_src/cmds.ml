(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 * The fact that kr_core is in its own file is simply because it's big.
 *)

open MapsSets
open Fam_batteries


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
  let t = Cmds_common.list_get_same_tree prl in
  let prel = 
    Cmds_common.prel_of_prl 
      ~is_weighted:(Mokaphy_prefs.Bary.weighted prefs)
      ~use_pp:(Mokaphy_prefs.Bary.use_pp prefs)
      prl
  in
  if prl <> [] then begin
    let fname = match Mokaphy_prefs.Bary.out_fname prefs with
      | "" -> (Cmds_common.cat_names prl)^".bary.xml"
      | s -> s
    in
    Phyloxml.named_tree_to_file
      (Cmds_common.chop_suffix_if_present fname ".xml") (* tree name *)
      (make_bary_tree t prel)
      fname
  end


(* *** HEAT HEAT HEAT HEAT HEAT *** *)
let heat prefs = function
  | [pr1; pr2] as prl ->
      let fname = match Mokaphy_prefs.Heat.out_fname prefs with
        | "" -> (Cmds_common.cat_names prl)^".heat.xml"
        | s -> s
      in
      let ref_tree = Placerun.get_same_tree pr1 pr2 
      and is_weighted = Mokaphy_prefs.Heat.weighted prefs
      and use_pp = Mokaphy_prefs.Heat.use_pp prefs
      in
      let tree_name = Cmds_common.chop_suffix_if_present fname ".xml"
      and my_pre_of_pr = Cmds_common.pre_of_pr ~is_weighted ~use_pp
      and refpkgo = 
        Cmds_common.refpkgo_of_fname (Mokaphy_prefs.Heat.refpkg_path prefs) 
      in
      Cmds_common.check_refpkgo_tree ref_tree refpkgo;
      Phyloxml.named_tree_list_to_file
        ([Some tree_name,
          Heat_tree.make_heat_tree prefs 
            (match refpkgo with
            | None -> Decor_gtree.of_newick_gtree ref_tree 
            | Some rp -> Refpkg.get_tax_ref_tree rp)
            (my_pre_of_pr pr1) 
            (my_pre_of_pr pr2)]
        @ match refpkgo with
        | None -> []
        | Some rp -> begin
            let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
            let my_make_tax_pre = 
              Cmds_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap in
            [Some (tree_name^".tax"),
            Heat_tree.make_heat_tree prefs taxt 
              (my_make_tax_pre pr1)
              (my_make_tax_pre pr2)]
        end)
        fname
  | [] -> () (* e.g. heat -help *)
  | _ -> failwith "Please specify exactly two place files to make a heat tree."


(* *** KR KR KR KR KR *** *)
let kr prefs prl = 
  Cmds_common.wrap_output (Mokaphy_prefs.KR.out_fname prefs)
    (fun ch -> Kr_core.core ch prefs prl)


(* *** PD PD PD PD PD *** *)
let pd prefs prl = 
  let pd_cmd = 
    if Mokaphy_prefs.PD.normalized prefs then Pd.normalized_of_pr
    else Pd.of_pr
  in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.PD.out_fname prefs) 
    (Cmds_common.write_unary 
      (pd_cmd (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PD.use_pp prefs)))
      prl)


(* *** PDFRAC PDFRAC PDFRAC PDFRAC PDFRAC *** *)
let pdfrac prefs prl = 
  let t = Cmds_common.list_get_same_tree prl
  and pra = Array.of_list prl
  in
  let inda = 
    Array.map
      (Induced.of_placerun 
        (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PDFrac.use_pp prefs)))
      pra
  in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.PDFrac.out_fname prefs) 
    (Cmds_common.write_uptri
      (Mokaphy_prefs.PDFrac.list_output prefs)
      (Array.map Placerun.get_name pra)
      "pdfrac"
      (Uptri.init
        (Array.length inda)
        (fun i j -> Pdfrac.of_induceds t inda.(i) inda.(j))))



(* *** AVGDIST AVGDIST AVGDIST AVGDIST AVGDIST  *** *)

let make_dist_fun prefs prl = 
  Pquery_distances.dist_fun_of_expon_weight 
    (Mokaphy_prefs.Avgdst.exponent prefs)
    (Mokaphy_prefs.weighting_of_bool (Mokaphy_prefs.Avgdst.weighted prefs))
    (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.Avgdst.use_pp prefs))
    (Edge_rdist.build_ca_info (Cmds_common.list_get_same_tree prl))

let uavgdst prefs prl = 
  Cmds_common.wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (Cmds_common.write_unary
      (Avgdst.of_placerun 
        (make_dist_fun prefs prl))
      prl)

let bavgdst prefs prl = 
  let pra = Array.of_list prl in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (Cmds_common.write_uptri
      (Mokaphy_prefs.Avgdst.list_output prefs)
      (Array.map Placerun.get_name pra)
      "bavgdst"
      (Uptri.init
        (Array.length pra)
        (fun i j -> 
          Avgdst.of_placerun_pair 
            (make_dist_fun prefs prl)
            pra.(i) 
            pra.(j))))


(* *** CLUSTER CLUSTER CLUSTER CLUSTER CLUSTER *** *)

open Clusterfunc

let t_prel_of_prl ~is_weighted ~use_pp prl = 
  (Cmds_common.list_get_same_tree prl,
    List.map (Cmds_common.pre_of_pr ~is_weighted ~use_pp) prl)

let tax_t_prel_of_prl ~is_weighted ~use_pp rp prl = 
  let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
  (taxt,
    List.map (Cmds_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap) prl)

let zeropad i = Printf.sprintf "%04d" i

let write_pre_tree drt id pre = 
  let tot = Mass_map.Pre.total_mass pre in
  assert(tot > 0.);
  Placeviz_core.write_fat_tree
   400. (* mass width *)
   1.   (* log coeff *)
   ((zeropad id)^".tre")
   drt
   (Mass_map.By_edge.of_pre ~factor:(1. /. tot) pre)

let mkdir path = 
  if 0 <> Sys.command ("mkdir "^path) then
    failwith ("unable to make directory "^path)

let cluster prefs prl = 
  let namel = List.map Placerun.get_name prl
  and is_weighted = Mokaphy_prefs.Cluster.weighted prefs
  and use_pp = Mokaphy_prefs.Cluster.use_pp prefs
  and refpkgo = 
    Cmds_common.refpkgo_of_fname (Mokaphy_prefs.Cluster.refpkg_path prefs) 
  and outdir = 
    match Mokaphy_prefs.Cluster.out_fname prefs with
    | "" -> failwith "please supply an output directory name"
    | s -> mkdir s; s
  and distf rt ~x1 ~x2 b1 b2 = 
    Kr_distance.dist_of_pres 1. rt ~x1 ~x2 ~pre1:b1 ~pre2:b2
  and normf a = 1. /. (Mass_map.Pre.total_mass a)
  in
  let (rt, prel) = t_prel_of_prl ~is_weighted ~use_pp prl
  in
  Cmds_common.check_refpkgo_tree rt refpkgo;
  let (drt, (cluster_t, blobim)) = 
    match refpkgo with
    | None -> 
      (Decor_gtree.of_newick_gtree rt,
      PreCluster.of_named_blobl (distf rt) normf
        (List.combine namel (List.map Mass_map.Pre.normalize_mass prel)))
    | Some rp -> begin
      let (taxt, tax_prel) = tax_t_prel_of_prl ~is_weighted ~use_pp rp prl in
      (taxt,
      PreCluster.of_named_blobl (distf taxt) normf
        (List.combine namel (List.map Mass_map.Pre.normalize_mass tax_prel)))
    end
  in
  Sys.chdir outdir;
  let ch = open_out "cluster.tre" in
  Newick.write ch cluster_t;
  close_out ch;
  mkdir "mass_trees";
  Sys.chdir "mass_trees";
  IntMap.iter (write_pre_tree drt) blobim;
  ()


(* *** SEECLUSTER SEECLUSTER SEECLUSTER SEECLUSTER SEECLUSTER *** *)
let seecluster prefs = function
  | [dirname1; dirname2] ->
      let prefix = Mokaphy_prefs.Seecluster.out_prefix prefs in
      if prefix = "" then failwith "Please specify an out prefix for seecluster"; 
      Seecluster.seecluster 
        prefix
        (Mokaphy_prefs.Seecluster.cutoff prefs)
        (dirname1^"/cluster.tre") 
        (dirname2^"/cluster.tre") 
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly two trees for seecluster."


(* *** CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ *** *)
let clusterviz prefs = function
  | [dirname1; dirname2] ->
      let cluster_fname = Mokaphy_prefs.Clusterviz.cluster_file prefs in
      if cluster_fname = "" then failwith "please specify a cluster file";
      Clusterviz.clusterviz cluster_fname dirname1 dirname2
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly two cluster directories for clusterviz."
