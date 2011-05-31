open MapsSets

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
    model       : Model.t Lazy.t;
    aln_fasta   : Alignment.t Lazy.t;
    aln_sto     : unit;
    aln_profile : unit;
    taxonomy    : Tax_taxonomy.t Lazy.t;
    seqinfom    : Tax_seqinfo.seqinfo_map Lazy.t;
    name        : string;
    (* inferred *)
    mrcam       : Tax_id.tax_id IntMap.t Lazy.t;
    uptree_map  : uptree_map Lazy.t;
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

(* deprecated now *)
let model_of_stats_fname prefs stats_fname ref_align =
  prefs.Prefs.stats_fname := stats_fname;
  Model.of_prefs "" prefs ref_align

(* This is the primary builder. We have the option of specifying an actual
 * alignment and a tree if we have them already. *)
let of_strmap ?ref_tree ?ref_align prefs m =
  let get what =
    try StringMap.find what m with
    | Not_found -> raise (Missing_element what)
  in
  let lfasta_aln =
    lazy
      (match ref_align with
      | Some a -> a
      | None -> Alignment_funs.upper_aln_of_any_file (get "aln_fasta"))
  in
  let lref_tree =
    lazy
      (match ref_tree with
      | Some t -> t
      | None -> Newick_gtree.of_file (get "tree_file"))
  and lmodel =
      lazy
        (let aln = Lazy.force lfasta_aln in
        if StringMap.mem "phylo_model_file" m then
          Model.of_json (StringMap.find "phylo_model_file" m) aln
        else begin
          print_endline
            "Warning: using a statistics file directly is now deprecated. \
            We suggest using a reference package. If you already are, then \
            please use the latest version of taxtastic.";
          model_of_stats_fname prefs (get "tree_stats") aln
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
  }

let of_path path =
  of_strmap (Prefs.defaults ()) (Refpkg_parse.strmap_of_path path)

(* *** ACCESSORIES *** *)

(* mrca tax decor, that is *)
let get_tax_decor_map rp =
  let td = get_taxonomy rp in
  IntMap.map
    (fun ti -> Decor.Taxinfo (ti, Tax_taxonomy.get_tax_name td ti))
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

let classify rp pr =
  Tax_classify.classify_pr
    Placement.add_classif
    (Tax_classify.classify
      (get_mrcam rp)
      (get_uptree_map rp))
    pr

let print_OK start rp =
  print_endline (start^(get_name rp)^" checks OK!")

(* make sure all of the tax maps etc are set up *)
let check_refpkg_classification rp =
  print_endline "Checking MRCA map...";
  let mrcam = get_mrcam rp in
  print_endline "Checking uptree map...";
  let utm = get_uptree_map rp in
  print_endline "Trying classifications...";
  let _ =
    List.map
      (Tax_classify.classify_loc mrcam utm)
      (Gtree.nonroot_node_ids (get_ref_tree rp))
  in
  ()

let check rp name what =
  print_endline ("Checking "^name^"...");
  let _ = what rp in ()

let check_tree_and_aln_names tree aln =
  let tns = StringSet.of_list (Newick_gtree.get_name_list tree)
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
  print_endline ("Checking refpkg "^(get_name rp)^"...");
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
  if 0 <> Newick_gtree.compare ~epsilon ~cmp_boot:false t (get_ref_tree rp) then
    failwith (title^" and the tree from "^(get_name rp)^" are not the same.")

let check_tree_approx = check_tree_identical ~epsilon:1e-5

let pr_check_tree_approx rp pr =
  check_tree_approx
    rp
    (pr.Placerun.name^" reference tree")
    (Placerun.get_ref_tree pr)
