open Ppatteries

exception Missing_element of string

(* uptree maps-- could be expanded later and go into a different file *)
type uptree_map = int IntMap.t

let utm_of_stree t =
  let m = ref IntMap.empty in
  let add_to_m i j = m := IntMap.check_add i j (!m) in
  let rec aux = function
    | Stree.Node (i, tL) ->
        List.iter (fun s -> add_to_m (Stree.top_id s) i; aux s) tL
    | Stree.Leaf _ -> ()
  in
  aux t;
  !m


(* Refpkg.t *)
type t =
  {
    (* specified *)
    ref_tree    : Newick_bark.newick_bark Gtree.gtree Lazy.t;
    model       : Glvm.t Lazy.t;
    aln_fasta   : Alignment.t Lazy.t;
    aln_sto     : unit;
    aln_profile : unit;
    taxonomy    : Tax_taxonomy.t Lazy.t;
    seqinfom    : Tax_seqinfo.seqinfo_map Lazy.t;
    name        : string;
    (* inferred *)
    mrcam       : Tax_id.tax_id IntMap.t Lazy.t;
    uptree_map  : uptree_map Lazy.t;
    item_path_fn: string -> string;
  }


(* *** basics *** *)

let get_ref_tree    rp = Lazy.force rp.ref_tree
let get_model       rp = Lazy.force rp.model
let get_aln_fasta   rp = Lazy.force rp.aln_fasta
let get_taxonomy    rp = Lazy.force rp.taxonomy
let get_seqinfom    rp = Lazy.force rp.seqinfom
let get_name        rp = rp.name
let get_mrcam       rp = Lazy.force rp.mrcam
let get_uptree_map  rp = Lazy.force rp.uptree_map
let get_item_path   rp = rp.item_path_fn

let set_ref_tree gt rp = {rp with ref_tree = lazy gt}
let set_aln_fasta aln rp = {rp with aln_fasta = lazy aln}

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
      let format_version = StringMap.find "format_version" m in
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
  let lfasta_aln =
    lazy
      (match ref_align with
      | Some a -> a
      | None ->
          Alignment.uppercase (Array.of_list (Fasta.of_file (get "aln_fasta")))
      )
  in
  let lref_tree =
    lazy
      (match ref_tree with
        | Some t -> t
        | None -> Newick_gtree.of_file (get "tree"))
  and lmodel = lazy
    (let aln = Lazy.force lfasta_aln in
     if StringMap.mem "phylo_model" m then
       let j = StringMap.find "phylo_model" m |> Json.of_file |> Jsontype.obj in
       match Hashtbl.find j "ras_model" |> Jsontype.string with
         | "gamma" ->
           (module Gmix_model.Model: Glvm.Model),
           Gmix_model.init_of_json j aln
         | "Price-CAT" ->
           (module Gcat_model.Model: Glvm.Model),
           Gcat_model.init_of_json j aln
         | x -> failwith ("invalid ras_model: " ^ x)
     else begin
       print_endline
         "Warning: using a statistics file directly is now deprecated. \
            We suggest using a reference package. If you already are, then \
            please use the latest version of taxtastic.";
       (module Gmix_model.Model: Glvm.Model),
       Gmix_model.init_of_stats_fname prefs (get "tree_stats") aln
     end)
  and ltaxonomy = lazy (Tax_taxonomy.of_ncbi_file (get "taxonomy"))
  and lseqinfom = lazy (Tax_seqinfo.of_csv (get "seq_info"))
  in
  let lmrcam =
    lazy (Tax_map.mrcam_of_data
           (Lazy.force lseqinfom)
           (Lazy.force ltaxonomy)
           (Lazy.force lref_tree))
  and luptree_map =
    lazy (utm_of_stree (Gtree.get_stree (Lazy.force lref_tree)))
  in
  {
    ref_tree    = lref_tree;
    model       = lmodel;
    aln_fasta   = lfasta_aln;
    aln_sto     = ();
    aln_profile = ();
    taxonomy    = ltaxonomy;
    seqinfom    = lseqinfom;
    name        = (get "name");
    mrcam       = lmrcam;
    uptree_map  = luptree_map;
    item_path_fn = get;
  }

let of_path ?ref_tree path =
  of_strmap ?ref_tree (Prefs.defaults ()) (Refpkg_parse.strmap_of_path path)

(* *** ACCESSORIES *** *)

(* mrca tax decor, that is *)
let get_tax_decor_map rp =
  let td = get_taxonomy rp in
  IntMap.filter_map
    (fun _ -> function
      | Tax_id.NoTax -> None
      | ti -> Some (Decor.Taxinfo (ti, Tax_taxonomy.get_tax_name td ti)))
    (get_mrcam rp)

(* tax ref tree is the usual ref tree with but with taxonomic annotation *)
let get_tax_ref_tree rp =
  Decor_gtree.add_decor_by_map
    (Decor_gtree.of_newick_gtree (get_ref_tree rp))
    (IntMap.map (fun x -> [x]) (get_tax_decor_map rp))

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
