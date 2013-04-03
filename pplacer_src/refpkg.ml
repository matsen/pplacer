open Ppatteries

exception Missing_element of string

(* Refpkg.t *)
type t =
  {
    (* specified *)
    ref_tree    : Newick_bark.newick_bark Gtree.gtree Delayed.t;
    model       : Glvm.t Delayed.t;
    aln_fasta   : Alignment.t Delayed.t;
    taxonomy    : Tax_taxonomy.t Delayed.t;
    seqinfom    : Tax_seqinfo.seqinfo_map Delayed.t;
    name        : string;
    (* inferred *)
    mrcam       : Tax_id.tax_id IntMap.t Delayed.t;
    uptree_map  : int IntMap.t Delayed.t;
    get         : string -> Refpkg_parse.contents;
    get_opt     : string -> Refpkg_parse.contents option;
    prefs       : Prefs.prefs;
  }


(* *** basics *** *)

let get_ref_tree rp =
  Delayed.reify rp.ref_tree (fun () ->
    rp.get "tree"
      |> Newick_gtree.of_refpkg_contents)

let get_aln_fasta rp =
  Delayed.reify rp.aln_fasta (fun () ->
    let aln = match rp.get_opt "aln_fasta" with
      | Some fa -> Fasta.of_refpkg_contents fa
      | None -> rp.get "aln_sto" |> Stockholm.of_refpkg_contents
    in
    Alignment.uppercase (Array.of_list aln))

let get_model rp =
  Delayed.reify rp.model (fun () ->
    let aln = get_aln_fasta rp in
    match rp.get_opt "phylo_model" with
    | Some v ->
      let j = Refpkg_parse.json_of_contents v |> Jsontype.obj in
      begin match Hashtbl.find j "ras_model" |> Jsontype.string with
      | "gamma" ->
        (module Gmix_model.Model: Glvm.Model),
        Gmix_model.init_of_json j aln
      | "Price-CAT" ->
        (module Gcat_model.Model: Glvm.Model),
        Gcat_model.init_of_json j aln
      | x -> failwith ("invalid ras_model: " ^ x)
      end
    | None ->
      print_endline
        "Warning: using a statistics file directly is now deprecated. \
         We suggest using a reference package. If you already are, then \
         please use the latest version of taxtastic.";
      (module Gmix_model.Model: Glvm.Model),
      Gmix_model.init_of_stats_fname
        rp.prefs
        (rp.get "tree_stats" |> Refpkg_parse.as_file_path)
        aln)

let get_taxonomy rp =
  Delayed.reify rp.taxonomy (fun () ->
    rp.get "taxonomy"
      |> Refpkg_parse.csv_of_contents
      |> with_dispose ~dispose:Csv.close_in Tax_taxonomy.of_ncbi_file)

let get_seqinfom rp =
  Delayed.reify rp.seqinfom (fun () ->
    rp.get "seq_info"
      |> Refpkg_parse.csv_of_contents
      |> with_dispose ~dispose:Csv.close_in Tax_seqinfo.of_csv)

let get_name rp = rp.name

let get_mrcam rp =
  Delayed.reify rp.mrcam (fun () ->
    Tax_map.mrcam_of_data
      (get_seqinfom rp)
      (get_taxonomy rp)
      (get_ref_tree rp))

let get_uptree_map rp =
  Delayed.reify rp.uptree_map (fun () ->
    get_ref_tree rp
      |> Gtree.get_stree
      |> Stree.parent_map)

let set_ref_tree gt rp = {rp with ref_tree = Delayed.init gt}
let set_aln_fasta aln rp = {rp with aln_fasta = Delayed.init aln}

let refpkg_versions = ["1.1"]

let show_supported_versions () =
  Printf.printf
    "Supported versions: %s.\n"
    (String.concat ", " refpkg_versions)

(* This is the primary builder. We have the option of specifying an actual
 * alignment and a tree if we have them already. *)
let of_strmap ?ref_tree ?ref_align ?(ignore_version = false) prefs m =
  let get what =
    try StringMap.find what m with
    | Not_found -> raise (Missing_element what)
  in
  if not ignore_version then begin
    if StringMap.mem "format_version" m then begin
      let format_version = StringMap.find "format_version" m
        |> Refpkg_parse.as_metadata
      in
      if not (List.mem format_version refpkg_versions) then begin
        Printf.printf
          "This reference package's format is version %s, which is not supported.\n"
          format_version;
        show_supported_versions ();
        invalid_arg "of_strmap"
      end
    end else begin
      print_endline
        "This reference package has no version information specified in it, \
      which most likely means it is an older, incompatible format.";
      show_supported_versions ();
      invalid_arg "of_strmap"
    end;
  end;
  {
    ref_tree = Delayed.of_option ref_tree;
    model = Delayed.create ();
    aln_fasta = Delayed.of_option ref_align;
    taxonomy = Delayed.create ();
    seqinfom = Delayed.create ();
    name = get "name" |> Refpkg_parse.as_metadata;
    mrcam = Delayed.create ();
    uptree_map = Delayed.create ();
    get; get_opt = flip StringMap.Exceptionless.find m;
    prefs;
  }

let of_path ?ref_tree path =
  of_strmap ?ref_tree (Prefs.defaults ()) (Refpkg_parse.strmap_of_path path)

(* *** ACCESSORIES *** *)

let tax_decor_map_of_mrcam td mrcam =
  IntMap.filter_map
    (fun _ -> function
      | Tax_id.NoTax -> None
      | ti -> Some (Decor.Taxinfo (ti, Tax_taxonomy.get_tax_name td ti)))
    mrcam

let tax_ref_tree_of_gtree td mrcam gt =
  Decor_gtree.add_decor_by_map
    (Decor_gtree.of_newick_gtree gt)
    (IntMap.map (fun x -> [x]) (tax_decor_map_of_mrcam td mrcam))

(* mrca tax decor, that is *)
let get_tax_decor_map rp =
  tax_decor_map_of_mrcam (get_taxonomy rp) (get_mrcam rp)

(* tax ref tree is the usual ref tree with but with taxonomic annotation *)
let get_tax_ref_tree ?alt_gt rp =
  let td = get_taxonomy rp in
  let mrcam, gt = match alt_gt with
    | Some gt -> Tax_map.mrcam_of_data (get_seqinfom rp) td gt, gt
    | None -> get_mrcam rp, get_ref_tree rp
  in
  tax_ref_tree_of_gtree td mrcam gt

(* if the rp is equipped with a taxonomy *)
let tax_equipped rp =
  try let _ = get_taxonomy rp and _ = get_seqinfom rp in true with
  | Missing_element _ -> false

let mrca_classify rp pr =
  Tax_classify.classify_pr
    Placement.add_classif
    (Tax_classify.mrca_classify
      (get_mrcam rp)
      (get_uptree_map rp))
    pr

let print_OK start rp =
  dprintf "%s%s checks OK!\n" start (get_name rp)

(* make sure all of the tax maps etc are set up *)
let check_refpkg_classification rp =
  dprint "Checking MRCA map...\n";
  let mrcam = get_mrcam rp in
  dprint "Checking uptree map...\n";
  let utm = get_uptree_map rp in
  dprint "Trying classifications...\n";
  let _ =
    List.map
      (Tax_classify.mrca_classify_loc mrcam utm)
      (Gtree.nonroot_node_ids (get_ref_tree rp))
  in
  ()

let check rp name what =
  dprintf "Checking %s...\n" name;
  let _ = what rp in ()

let check_tree_and_aln_names tree aln =
  let tns = Newick_gtree.leaf_label_map tree
    |> IntMap.values
    |> StringSet.of_enum
  and ans = StringSet.of_list (Array.to_list (Alignment.get_name_arr aln))
  in
  let test (s1, n1) (s2, n2) =
    let d = StringSet.diff s1 s2 in
    if not (StringSet.is_empty d) then begin
      Format.fprintf Format.str_formatter
        "present in %s but not %s: %a" n1 n2 StringSet.ppr d;
      failwith (Format.flush_str_formatter ())
    end
  in
  test (tns, "tree") (ans, "alignment");
  test (ans, "alignment") (tns, "tree");
  ()

let check_refpkg rp =
  dprintf "Checking refpkg %s...\n" (get_name rp);
  check rp "tree" get_ref_tree;
  check rp "model" get_model;
  check rp "alignment" get_aln_fasta;
  check_tree_and_aln_names (get_ref_tree rp) (get_aln_fasta rp);
  try
    check rp "taxonomy" get_taxonomy;
    check rp "seqinfom" get_seqinfom;
    check_refpkg_classification rp;
    print_OK "Taxonomically-informed reference package " rp
  with
    | Missing_element _ ->
        print_OK "Non-taxonomically-informed reference package " rp

(* check that a given tree t is the same as the ref tree in the refpkg rp.
 * Note that we don't check bootstraps. *)
let check_tree_identical ?epsilon:(epsilon=0.) rp title t =
  if 0 <> Newick_gtree.compare ~epsilon ~cmp_edge_label:false t (get_ref_tree rp) then
    failwith (title^" and the tree from "^(get_name rp)^" are not the same.")

let check_tree_approx = check_tree_identical ~epsilon:1e-5

let pr_check_tree_approx rp pr =
  check_tree_approx
    rp
    (pr.Placerun.name^" reference tree")
    (Placerun.get_ref_tree pr)

let check_tree_subset rp name gt =
  let leaves = Newick_gtree.leaf_label_map
    %> IntMap.values
    %> StringSet.of_enum
  in
  let ref_leaves = get_ref_tree rp |> leaves
  and subset_leaves = leaves gt in
  let non_overlapped = StringSet.diff subset_leaves ref_leaves in
  if not (StringSet.is_empty non_overlapped) then
    failwith
      (Printf.sprintf "%s is not a subset of %s. mismatched leaves include: %s"
         name
         (get_name rp)
         (StringSet.elements non_overlapped |> String.join ", " ))
