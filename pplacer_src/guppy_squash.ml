(* This is where squash clustering actually gets used.
 *
 * Note that in the definition below, the objects being clustered are
 * pairs of a number list and a pre. This means that identical pres
 * don't get lost when we use them as keys. *)

open Ppatteries
open Subcommand
open Guppy_cmdobjs
open Squashfunc

module NPreBlob =
  struct
    type t = int list * Mass_map.Pre.t
    let compare = Pervasives.compare
    let merge (l1, p1) (l2, p2) = (l1 @ l2, p1 @ p2)
  end

module NPreSquash = Squash (NPreBlob)

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

let mkdir path =
  try
    Unix.mkdir path 0o755
  with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(*
# numberize [5;4;3];;
- : (int list * int) list = [([0], 5); ([1], 4); ([2], 3)]
*)
let numberize l =
  (flip List.cons [] |- curry identity |> List.mapi) l

(* Note that denom_f is a function that gives us a denominator to normalize by
 * when calculating branch lengths, i.e. the setting of the --normalize flag.
 * Called denom here to avoid confusion with the normalization of the mass. *)
let make_cluster p denom_f weighting criterion refpkgo mode_str prl =
  let namel = List.map Placerun.get_name prl
  and distf denom rt ~x1 ~x2 (_,b1) (_,b2) =
    (Kr_distance.dist_of_pres p rt ~x1 ~x2 ~pre1:b1 ~pre2:b2) /. denom
  and normf (_,a) = 1. /. (Mass_map.Pre.total_mass a)
  in
  let (rt, prel) = t_prel_of_prl weighting criterion prl
  and prep prel =
    List.combine
      namel
      (numberize (List.map Mass_map.Pre.normalize_mass prel))
  in
  let (drt, (cluster_t, numbered_blobim)) =
    if mode_str = "" then begin
      (* phylogenetic clustering *)
      let denom = denom_f (rt :> Newick_gtree.t) in
      (Decor_gtree.of_newick_gtree rt,
        NPreSquash.of_named_blobl (distf denom rt) normf (prep prel))
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
        let denom = denom_f (taxt :> Newick_gtree.t) in
        (taxt,
          NPreSquash.of_named_blobl (distf denom taxt) normf (prep tax_prel))
    end
  in
  (drt, cluster_t, IntMap.map snd numbered_blobim)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit rng_cmd () as super_rng
  inherit fat_cmd () as super_fat
  inherit placefile_cmd () as super_placefile
  inherit output_cmd ~show_fname:false () as super_output
  inherit kr_cmd () as super_kr
  inherit normalization_cmd () as super_normalization

  val nboot = flag "--bootstrap"
    (Plain (0, "the number of bootstrap replicates to run"))
  val tax_cluster_mode = flag "--tax-cluster"
    (Plain ("", "Perform taxonomic clustering rather than phylogenetic.\
    Specify \"unit\" or \"inv\" for the two different modes."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_rng#specl
    @ super_fat#specl
    @ super_output#specl
    @ super_kr#specl
    @ super_normalization#specl
    @ [
      int_flag nboot;
      string_flag tax_cluster_mode
    ]

  method desc =
"performs squash clustering"
  method usage = "usage: squash [options] placefiles"

  method private write_pre_tree prefix infix drt pre =
    let tot = Mass_map.Pre.total_mass pre in
    assert(tot > 0.);
    let tree_name = prefix^"."^infix in
    let massm = (Mass_map.By_edge.of_pre ~factor:(1. /. tot) pre) in
    Phyloxml.named_gtree_to_file
      (tree_name ^ ".fat.xml")
      (tree_name ^ ".fat")
      (self#fat_tree_of_massm drt massm)

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts
    and refpkgo = self#get_rpo
    and mode_str = fv tax_cluster_mode
    and zero_pad_int width i =
      String_matrix.pad_to_width '0' width (string_of_int i)
    and p = fv p_exp
    and denom_f = self#get_normalization
    in
    let our_make_cluster =
      make_cluster p denom_f weighting criterion in
    let path = (^) (self#single_prefix ()) in
    let nboot = fv nboot in
    self#check_placerunl prl;
    if 0 = nboot then begin
      (* bootstrap turned off *)
      let (drt, cluster_t, blobim) = our_make_cluster refpkgo mode_str prl in
      Newick_gtree.to_file cluster_t (path Squash_common.cluster_tree_name);
      let outdir = path Squash_common.mass_trees_dirname in mkdir outdir;
      let pad_width = find_zero_pad_width (IntMap.cardinal blobim) in
      let prefix_of_int i = Filename.concat outdir (zero_pad_int pad_width i) in
      (* make a tax tree here then run mimic on it *)
      let wpt infix t i =
        self#write_pre_tree (prefix_of_int i) infix t
      in
      match refpkgo with
        | None -> IntMap.iter (wpt "phy" drt) blobim
        | Some rp ->
        (* use a tax-labeled ref tree. Note that we've already run check_refpkgo_tree *)
          let tdrt = Refpkg.get_tax_ref_tree rp in
          IntMap.iter (wpt "phy" tdrt) blobim;
          let (taxt, tax_prel) =
            tax_t_prel_of_prl
              Tax_gtree.of_refpkg_unit weighting criterion rp prl in
          let tax_blobim =
            IntMap.map snd (NPreSquash.mimic cluster_t (numberize tax_prel))
          in
          IntMap.iter (wpt "tax" taxt) tax_blobim
    end
    else begin
      let pad_width = find_zero_pad_width nboot in
      let rng = self#rng in
      for i=1 to nboot do
        Printf.printf "running bootstrap %d of %d\n" i nboot;
        let boot_prl = List.map (Bootstrap.boot_placerun rng) prl in
        let (_, cluster_t, _) = our_make_cluster refpkgo mode_str boot_prl in
        Newick_gtree.to_file
          cluster_t
          (path ("cluster."^(zero_pad_int pad_width i)^".tre"))
      done
    end
end
