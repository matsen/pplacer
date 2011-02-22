
open Clusterfunc
open MapsSets

(* CLUSTER CLUSTER CLUSTER CLUSTER CLUSTER CLUSTER CLUSTER CLUSTER *)
module Prefs = struct
  type mokaphy_prefs =
    {
      out_fname: string ref;
      use_pp: bool ref;
      weighted: bool ref;
      refpkg_path : string ref;
      nboot : int ref;
      seed : int ref;
      tax_cluster_mode : string ref;
      transform : string ref;
    }

  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let weighted          p = !(p.weighted)
  let refpkg_path       p = !(p.refpkg_path)
  let nboot             p = !(p.nboot)
  let seed              p = !(p.seed)
  let tax_cluster_mode  p = !(p.tax_cluster_mode)
  let transform         p = !(p.transform)

  let defaults () =
    {
      out_fname = ref "";
      use_pp = ref false;
      weighted = ref false;
      refpkg_path = ref "";
      nboot = ref 0;
      seed = ref 0;
      tax_cluster_mode = ref "";
      transform = ref "";
    }

  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "-c", Arg.Set_string prefs.refpkg_path,
    (Mokaphy_common.refpkg_help "cluster");
    "--unweighted", Arg.Clear prefs.weighted,
    Mokaphy_common.weighted_help;
    "--bootstrap", Arg.Set_int prefs.nboot,
    "the number of bootstrap replicates to run";
    "--seed", Arg.Set_int prefs.seed,
    "Random seed for bootstrap";
    "--tax-cluster", Arg.Set_string prefs.tax_cluster_mode,
    "Perform taxonomic clustering rather than phylogenetic.\
    Specify \"unit\" or \"inv\" for the two different modes.";
    "--transform", Arg.Set_string prefs.transform,
    Mokaphy_common.transform_help;
    ]
end


let classify_mode_str = function
  | "unit" -> Tax_gtree.of_refpkg_unit
  | "inv" -> Tax_gtree.of_refpkg_inverse
  | s -> failwith ("unknown tax cluster mode: "^s)

let t_prel_of_prl ~is_weighted ~use_pp prl =
  (Mokaphy_common.list_get_same_tree prl,
    List.map (Mokaphy_common.pre_of_pr ~is_weighted ~use_pp) prl)

let tax_t_prel_of_prl tgt_fun ~is_weighted ~use_pp rp prl =
  let (taxt, ti_imap) = tgt_fun rp in
  (taxt,
    List.map (Mokaphy_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap) prl)

let zeropad i = Printf.sprintf "%04d" i

let write_pre_tree transform infix drt id pre =
  let tot = Mass_map.Pre.total_mass transform pre in
  assert(tot > 0.);
  Placeviz_core.write_fat_tree
    ~min_bl:2e-2
    400. (* mass width *)
    1.   (* log coeff *)
    ((zeropad id)^"."^infix)
    drt
    (Mass_map.By_edge.of_pre transform ~factor:(1. /. tot) pre)

let mkdir path =
  if 0 <> Sys.command ("mkdir "^path) then
    failwith ("unable to make directory "^path)

let make_cluster transform ~is_weighted ~use_pp refpkgo prefs prl =
  let namel = List.map Placerun.get_name prl
  and distf rt ~x1 ~x2 b1 b2 =
    Kr_distance.dist_of_pres transform 1. rt ~x1 ~x2 ~pre1:b1 ~pre2:b2
  and normf a = 1. /. (Mass_map.Pre.total_mass transform a)
  in
  let (rt, prel) = t_prel_of_prl ~is_weighted ~use_pp prl
  in
  let mode_str = Prefs.tax_cluster_mode prefs in
  Mokaphy_common.check_refpkgo_tree rt refpkgo;
  let (drt, (cluster_t, blobim)) =
    if mode_str = "" then begin
      (* phylogenetic clustering *)
      (Decor_gtree.of_newick_gtree rt,
      PreCluster.of_named_blobl (distf rt) normf
        (List.combine
          namel
          (List.map (Mass_map.Pre.normalize_mass transform) prel)))
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
          PreCluster.of_named_blobl
            (distf taxt)
            normf
            (List.combine
              namel
              (List.map (Mass_map.Pre.normalize_mass transform) tax_prel)))
    end
  in
  (drt, cluster_t, blobim)

let cluster prefs prl =
  let () =
    match Prefs.out_fname prefs with
    | "" -> failwith "please supply an output directory name"
    | s -> mkdir s; Sys.chdir s
  and is_weighted = Prefs.weighted prefs
  and use_pp = Prefs.use_pp prefs
  and transform =
    Mass_map.transform_of_str (Prefs.transform prefs)
  and refpkgo =
    Mokaphy_common.refpkgo_of_fname (Prefs.refpkg_path prefs)
  in
  let nboot = Prefs.nboot prefs in
  let width = Base.find_zero_pad_width nboot in
  let pad_str_of_int i =
    String_matrix.pad_to_width '0' width (string_of_int i)
  in
  if 0 = nboot then begin
    (* bootstrap turned off *)
    let (drt, cluster_t, blobim) =
      make_cluster transform ~is_weighted ~use_pp refpkgo prefs prl in
    Newick_gtree.to_file cluster_t Cluster_common.cluster_tree_name;
    mkdir Cluster_common.mass_trees_dirname;
    Sys.chdir Cluster_common.mass_trees_dirname;
    (* make a tax tree here then run mimic on it *)
    match refpkgo with
    | None -> IntMap.iter (write_pre_tree transform "phy" drt) blobim
    | Some rp ->
(* use a tax-labeled ref tree. Note that we've already run check_refpkgo_tree *)
      let tdrt = Refpkg.get_tax_ref_tree rp in
      IntMap.iter (write_pre_tree transform "phy" tdrt) blobim;
      let (taxt, tax_prel) =
        tax_t_prel_of_prl
          Tax_gtree.of_refpkg_unit ~is_weighted ~use_pp rp prl in
      let tax_blobim = PreCluster.mimic cluster_t tax_prel in
      IntMap.iter (write_pre_tree Mass_map.no_transform "tax" taxt) tax_blobim
  end
  else begin
    let rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
    Gsl_rng.set rng (Nativeint.of_int (Prefs.seed prefs));
    for i=1 to nboot do
      Printf.printf "running bootstrap %d of %d\n" i nboot;
      let boot_prl = List.map (Bootstrap.boot_placerun rng) prl in
      let (_, cluster_t, _) =
        make_cluster transform ~is_weighted ~use_pp refpkgo prefs boot_prl in
      Newick_gtree.to_file cluster_t ("cluster."^(pad_str_of_int i)^".tre");
      (* run distance on bootstraps *)
      let kr_prefs = Mokaphy_kr.Prefs.defaults () in
      kr_prefs.Mokaphy_kr.Prefs.list_output := true;
      kr_prefs.Mokaphy_kr.Prefs.out_fname :=
        ("dist."^(pad_str_of_int i)^".tab");
      Mokaphy_kr.kr kr_prefs boot_prl;
    done
  end

