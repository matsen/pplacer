open Subcommand
open Guppy_cmdobjs
open Clusterfunc
open MapsSets

let classify_mode_str = function
  | "unit" -> Tax_gtree.of_refpkg_unit
  | "inv" -> Tax_gtree.of_refpkg_inverse
  | s -> failwith ("unknown tax cluster mode: "^s)

let t_prel_of_prl weighting criterion prl =
  (Mokaphy_common.list_get_same_tree prl,
    List.map (Mass_map.Pre.of_placerun weighting criterion) prl)

let tax_t_prel_of_prl tgt_fun weighting criterion rp prl =
  let (taxt, ti_imap) = tgt_fun rp in
  (taxt,
    List.map (Mokaphy_common.make_tax_pre taxt weighting criterion ti_imap) prl)

let zeropad i = Printf.sprintf "%04d" i

let write_pre_tree transform prefix infix drt id pre =
  let tot = Mass_map.Pre.total_mass transform pre in
  assert(tot > 0.);
  Placeviz_core.write_fat_tree
    ~min_bl:2e-2
    400. (* mass width *)
    1.   (* log coeff *)
    (prefix^((zeropad id)^"."^infix))
    drt
    (Mass_map.By_edge.of_pre transform ~factor:(1. /. tot) pre)

let mkdir path =
  try
    Unix.mkdir path 0o755
  with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let make_cluster transform weighting criterion refpkgo mode_str prl =
  let namel = List.map Placerun.get_name prl
  and distf rt ~x1 ~x2 b1 b2 =
    Kr_distance.dist_of_pres transform 1. rt ~x1 ~x2 ~pre1:b1 ~pre2:b2
  and normf a = 1. /. (Mass_map.Pre.total_mass transform a)
  in
  let (rt, prel) = t_prel_of_prl weighting criterion prl
  in
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
            weighting criterion rp prl in
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

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd () as super_refpkg
  inherit rng_cmd () as super_rng
  inherit placefile_cmd () as super_placefile

  val outdir = flag "-o"
    (Needs_argument ("output directory", "The output directory. Required"))
  val nboot = flag "--bootstrap"
    (Plain (0, "the number of bootstrap replicates to run"))
  val tax_cluster_mode = flag "--tax-cluster"
    (Plain ("", "Perform taxonomic clustering rather than phylogenetic.\
    Specify \"unit\" or \"inv\" for the two different modes."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_rng#specl
    @ [
      string_flag outdir;
      int_flag nboot;
      string_flag tax_cluster_mode
    ]

  method desc = ""
  method usage = ""

  method private placefile_action prl =
    let outdir = fv outdir in mkdir outdir;
    let transform, weighting, criterion = self#mass_opts
    and refpkgo = Mokaphy_common.refpkgo_of_fname (fv refpkg_path)
    and mode_str = fv tax_cluster_mode
    in
    let path = Filename.concat outdir in
    let nboot = fv nboot in
    let width = Base.find_zero_pad_width nboot in
    let pad_str_of_int i =
      String_matrix.pad_to_width '0' width (string_of_int i)
    in
    if 0 = nboot then begin
      (* bootstrap turned off *)
      let (drt, cluster_t, blobim) =
        make_cluster transform weighting criterion refpkgo mode_str prl in
      Newick_gtree.to_file cluster_t (path Cluster_common.cluster_tree_name);
      let outdir = path Cluster_common.mass_trees_dirname in mkdir outdir;
      let path = Filename.concat outdir in
      (* make a tax tree here then run mimic on it *)
      match refpkgo with
        | None -> IntMap.iter (write_pre_tree transform (path "") "phy" drt) blobim
        | Some rp ->
        (* use a tax-labeled ref tree. Note that we've already run check_refpkgo_tree *)
          let tdrt = Refpkg.get_tax_ref_tree rp in
          IntMap.iter (write_pre_tree transform (path "") "phy" tdrt) blobim;
          let (taxt, tax_prel) =
            tax_t_prel_of_prl
              Tax_gtree.of_refpkg_unit weighting criterion rp prl in
          let tax_blobim = PreCluster.mimic cluster_t tax_prel in
          IntMap.iter (write_pre_tree Mass_map.no_transform (path "") "tax" taxt) tax_blobim
    end
    else begin
      let rng = self#rng in
      for i=1 to nboot do
        Printf.printf "running bootstrap %d of %d\n" i nboot;
        let boot_prl = List.map (Bootstrap.boot_placerun rng) prl in
        let (_, cluster_t, _) =
          make_cluster transform weighting criterion refpkgo mode_str boot_prl in
        Newick_gtree.to_file cluster_t (path ("cluster."^(pad_str_of_int i)^".tre"))
      done
    end


end
