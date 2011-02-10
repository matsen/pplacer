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

let classify_mode_str = function
  | "unit" -> Tax_gtree.of_refpkg_unit
  | "inv" -> Tax_gtree.of_refpkg_inverse
  | s -> failwith ("unknown tax cluster mode: "^s)

let t_prel_of_prl ~is_weighted ~use_pp prl = 
  (Cmds_common.list_get_same_tree prl,
    List.map (Cmds_common.pre_of_pr ~is_weighted ~use_pp) prl)

let tax_t_prel_of_prl tgt_fun ~is_weighted ~use_pp rp prl = 
  let (taxt, ti_imap) = tgt_fun rp in
  (taxt,
    List.map (Cmds_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap) prl)

let zeropad i = Printf.sprintf "%04d" i

let write_pre_tree infix drt id pre = 
  let tot = Mass_map.Pre.total_mass pre in
  assert(tot > 0.);
  Placeviz_core.write_fat_tree
    ~min_bl:2e-2
    400. (* mass width *)
    1.   (* log coeff *)
    ((zeropad id)^"."^infix)
    drt 
    (Mass_map.By_edge.of_pre ~factor:(1. /. tot) pre)

let mkdir path = 
  if 0 <> Sys.command ("mkdir "^path) then
    failwith ("unable to make directory "^path)

let make_cluster ~is_weighted ~use_pp refpkgo prefs prl = 
  let namel = List.map Placerun.get_name prl
  and distf rt ~x1 ~x2 b1 b2 = 
    Kr_distance.dist_of_pres 1. rt ~x1 ~x2 ~pre1:b1 ~pre2:b2
  and normf a = 1. /. (Mass_map.Pre.total_mass a)
  in
  let (rt, prel) = t_prel_of_prl ~is_weighted ~use_pp prl
  in
  let mode_str = Mokaphy_prefs.Cluster.tax_cluster_mode prefs in
  Cmds_common.check_refpkgo_tree rt refpkgo;
  let (drt, (cluster_t, blobim)) = 
    if mode_str = "" then begin
      (* phylogenetic clustering *)
      (Decor_gtree.of_newick_gtree rt,
      PreCluster.of_named_blobl (distf rt) normf
        (List.combine namel (List.map Mass_map.Pre.normalize_mass prel)))
    end
    else
      (* taxonomic clustering *)
      match refpkgo with
      | None -> failwith "taxonomic clustering requested but no reference package supplied"
      | Some rp -> begin
        let (taxt, tax_prel) = 
          tax_t_prel_of_prl 
            (classify_mode_str mode_str)
            ~is_weighted ~use_pp rp prl in
        (taxt,
          PreCluster.of_named_blobl (distf taxt) normf (List.combine namel (List.map Mass_map.Pre.normalize_mass tax_prel))) end
  in
  (drt, cluster_t, blobim)

let cluster prefs prl = 
  let () = 
    match Mokaphy_prefs.Cluster.out_fname prefs with
    | "" -> failwith "please supply an output directory name"
    | s -> mkdir s; Sys.chdir s
  and is_weighted = Mokaphy_prefs.Cluster.weighted prefs
  and use_pp = Mokaphy_prefs.Cluster.use_pp prefs
  and refpkgo = 
    Cmds_common.refpkgo_of_fname (Mokaphy_prefs.Cluster.refpkg_path prefs) 
  in
  let nboot = Mokaphy_prefs.Cluster.nboot prefs in
  let width = Base.find_zero_pad_width nboot in
  let pad_str_of_int i = 
    String_matrix.pad_to_width '0' width (string_of_int i)
  in
  if 0 = nboot then begin
    (* bootstrap turned off *)
    let (drt, cluster_t, blobim) = 
      make_cluster ~is_weighted ~use_pp refpkgo prefs prl in
    Newick.to_file cluster_t Cluster_common.cluster_tree_name;
    mkdir Cluster_common.mass_trees_dirname;
    Sys.chdir Cluster_common.mass_trees_dirname;
    (* make a tax tree here then run mimic on it *)
    match refpkgo with
    | None -> IntMap.iter (write_pre_tree "phy" drt) blobim
    | Some rp ->
(* use a tax-labeled ref tree. Note that we've already run check_refpkgo_tree *)
      let tdrt = Refpkg.get_tax_ref_tree rp in
      IntMap.iter (write_pre_tree "phy" tdrt) blobim;
      let (taxt, tax_prel) = 
        tax_t_prel_of_prl 
          Tax_gtree.of_refpkg_unit ~is_weighted ~use_pp rp prl in
      let tax_blobim = PreCluster.mimic cluster_t tax_prel in
      IntMap.iter (write_pre_tree "tax" taxt) tax_blobim 
  end
  else begin
    let () = Random.init (Mokaphy_prefs.Cluster.seed prefs) 
    and rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002
    in
    Gsl_rng.set rng (Nativeint.of_int (Mokaphy_prefs.Cluster.seed prefs));
    for i=1 to nboot do
      Printf.printf "running bootstrap %d of %d\n" i nboot;
      let boot_prl = List.map (Bootstrap.boot_placerun rng) prl in
      let (_, cluster_t, _) = 
        make_cluster ~is_weighted ~use_pp refpkgo prefs boot_prl in
      Newick.to_file cluster_t ("cluster."^(pad_str_of_int i)^".tre");
      (* run distance on bootstraps *)
      let kr_prefs = Mokaphy_prefs.KR.defaults () in
      kr_prefs.Mokaphy_prefs.KR.list_output := true;
      kr_prefs.Mokaphy_prefs.KR.out_fname := 
        ("dist."^(pad_str_of_int i)^".tab");
      kr kr_prefs boot_prl;
    done
  end

(* *** CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ *** *)

(* get mass tree(s) and name them with name *)
let get_named_mass_tree dirname i name = 
  List.flatten
    (List.map
      (fun infix ->
        let fname = 
          dirname^"/"^Cluster_common.mass_trees_dirname
            ^"/"^(zeropad i)^infix^".fat.xml" in
        if Sys.file_exists fname then
          [{
            (List.hd
              (Xphyloxml.load fname).Xphyloxml.trees)
            with Xphyloxml.name = Some (name^infix)
          }]
        else [])
      [".phy"; ".tax"])

let clusterviz prefs = function
  | [dirname] -> begin
      match (Mokaphy_prefs.Clusterviz.name_csv prefs,
             Mokaphy_prefs.Clusterviz.out_fname prefs) with
        | "", _ -> failwith "please specify a cluster CSV file"
        | _, "" -> failwith "please specify an out file name"
        | (cluster_fname, out_fname) -> 
          try
            let nameim = Cluster_common.nameim_of_csv cluster_fname 
            and out_tree_name = 
              Cmds_common.chop_suffix_if_present cluster_fname ".csv"
            in
            let (nt, ssm) = Clusterviz.name_tree_and_subsets_map dirname nameim in
            (* write it out, and read it back in for the combination *)
            let ch = open_out out_fname in
            Phyloxml.write_named_tree_list ch [Some out_tree_name, nt];
            close_out ch;
            let named_tree = 
              match (Xphyloxml.load out_fname).Xphyloxml.trees with
              | [t] -> t
              | _ -> assert(false)
            in
            (* now we read in the cluster trees *)
            let mass_trees = 
              List.map 
                (fun (i, name) -> get_named_mass_tree dirname i name)
                (IntMapFuns.to_pairs nameim)
            in
            (* write out the average masses corresponding to the named clusters *)
            Xphyloxml.pxdata_to_file out_fname 
              { Xphyloxml.trees = (named_tree::(List.flatten mass_trees)); 
                Xphyloxml.data_attribs = Xphyloxml.phyloxml_attrs; };
            (* write out CSV file showing the cluster contents *)
            let subsets_fname = Mokaphy_prefs.Clusterviz.subsets_fname prefs in
            if subsets_fname <> "" then 
              Csv.save subsets_fname 
                (StringMap.fold
                  (fun name s l -> 
                    [name; String.concat "," (StringSet.elements s)]::l)
                  ssm
                  [])
          with Clusterviz.Numbering_mismatch ->
            failwith ("numbering mismatch with "^dirname^" and "^cluster_fname)
  end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster directory for clusterviz."


(* *** BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ *** *)
let bootviz prefs = function
  | [ct_fname] ->
      let out_fname = match Mokaphy_prefs.Bootviz.out_fname prefs with
        | "" -> "cluster_boot.xml" 
        | s -> s
      in
      Phyloxml.tree_to_file
        (Bootviz.decorate_tree 
          (Mokaphy_prefs.Bootviz.cutoff prefs) 
          (Mokaphy_prefs.Bootviz.boot_fname prefs)
          ct_fname)
        out_fname
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootviz."


(* *** BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB *** *)
let bootsub prefs = function
  | [ct_fname] -> begin
      match (Mokaphy_prefs.Bootsub.boot_fname prefs,
            Mokaphy_prefs.Bootsub.name_csv prefs) with
        | "",_ -> failwith "please supply a file of bootstrapped trees"
        | _,"" -> failwith "please supply a cluster CSV file"
        | (boot_fname, csv_fname) -> 
            Cmds_common.wrap_output (Mokaphy_prefs.Bootsub.out_fname prefs)
              (fun ch ->
                Bootsub.perform 
                  ch
                  (Mokaphy_prefs.Bootsub.cutoff prefs) 
                  ~csv_fname
                  ~boot_fname
                  ~ct_fname)
      end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootsub."


(* *** PCA PCA PCA PCA PCA *** *)
let pca prefs = function
  | [] -> ()
  | prl ->
      match Mokaphy_prefs.Pca.out_prefix prefs with 
      | "" -> failwith "Please specify an out prefix for pca with -o"
      | prefix ->
      Pca.pca_complete 
        ~scale:(Mokaphy_prefs.Pca.scale prefs)
        (Mokaphy_prefs.weighting_of_bool (Mokaphy_prefs.Pca.weighted prefs))
        (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.Pca.use_pp prefs))
        (Mokaphy_prefs.Pca.multiplier prefs)
        (Mokaphy_prefs.Pca.write_n prefs)
        (Cmds_common.refpkgo_of_fname (Mokaphy_prefs.Pca.refpkg_path prefs))
        prefix
        prl
