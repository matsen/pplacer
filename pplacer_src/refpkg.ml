(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let refpkg_str = "CONTENTS.txt"

(* uptree maps-- could be expanded later and go into a different file *)
type uptree_map = int IntMap.t

let utm_of_stree t = 
  let m = ref IntMap.empty in
  let add_to_m i j = m := IntMapFuns.check_add i j (!m) in
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

(* *** parsing *** *)

(* (nonempty) then space then = then (nothing or something ending in something
 * nonempty) then space *)
let equality_rex = 
  Str.regexp "\\([^ \t]+\\)[ \t]+=[ \t]*\\(\\|.*[^ \t]\\)[ \t]*$"

let eqpair_of_str s = 
  if Str.string_match equality_rex s 0 then
    (Str.matched_group 1 s, Str.matched_group 2 s)
  else
    failwith ("equality_pair_of_str: malformed string: "^s)

let eqmap_of_strl sl = 
  List.fold_right
    (fun (k,v) -> StringMap.add k v)
    (List.map eqpair_of_str sl)
    StringMap.empty

 (* parsing sectioned files *)

let sechead_rex = Str.regexp "^[ \t]*\\[\\([^]]*\\)\\][ \t]*"

let extract_secheado s = 
  if not (Str.string_match sechead_rex s 0) then None
  else Some (Str.matched_group 1 s)

let secmap_of_strl strl = 
  let rec aux m curr_sec = function
    | x::l -> 
        (match extract_secheado x with
        | Some sec -> aux m sec l
        | None -> aux (StringMapFuns.add_listly curr_sec x m) curr_sec l)
    | [] -> m
  in
  match File_parsing.filter_empty_lines strl with
    | x::l ->
        (match extract_secheado x with
        | Some sec -> StringMap.map List.rev (aux StringMap.empty sec l)
        | None -> invalid_arg "You must start config file with a section header!")
    | [] -> StringMap.empty


(* NOTE: at a later date, we may want to transition to a system whereby the
 * stats file is parsed directly, rather than having it set the prefs then doing
 * an of_prefs. *)
let build_model stats_fname ref_align = 
  print_endline stats_fname;
  let prefs = Prefs.defaults () in
  prefs.Prefs.stats_fname := stats_fname;
  Model.of_prefs "" prefs ref_align

let remove_terminal_slash s = 
  let len = String.length s in
  if s.[len - 1] <> '/' then s
  else String.sub s 0 (len-1)


(* final product *)
let of_path path = 
  if not (Sys.is_directory path) then
    failwith ("Purported refpkg "^path^" is not a directory");
  let noslash = remove_terminal_slash path in
  let dirize fname = noslash^"/"^fname in
  let secmap = 
    secmap_of_strl 
      (try File_parsing.string_list_of_file (dirize refpkg_str) with
      | Sys_error _ -> invalid_arg (Printf.sprintf "can't find %s in %s" refpkg_str path))
  in
  let get_sec s = 
    try StringMap.find s secmap with
    | Not_found -> invalid_arg ("missing section "^s^" in refpkg "^path) 
  in
  let filemap = eqmap_of_strl (get_sec "files") in
  (* pull from the filemap *)
  let get what = 
    try StringMap.find what filemap with
    | Not_found -> invalid_arg (what^" not found in "^(dirize refpkg_str))
  in
  (* for when the what is actually a file *)
  let dget what = 
    match get what with
    | "" -> invalid_arg ("Please specify a "^what^" in your refpkg "^path)
    | s -> dirize s
  in
  (* now we make the inferred things *)
  let lfasta_aln = lazy (Alignment.read_fasta(dget "aln_fasta")) in
  let lref_tree = lazy (Newick.of_file (dget "tree_file")) 
  and lmodel = 
      lazy (build_model (dget "tree_stats") (Lazy.force lfasta_aln));
  and ltaxonomy = lazy (Tax_taxonomy.of_ncbi_file (dget "taxonomy"))
  and lseqinfom = lazy (Tax_seqinfo.of_csv (dget "seq_info"))
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
    name        = Filename.chop_extension (Filename.basename noslash);
    mrcam       = lmrcam;
    uptree_map  = luptree_map;
  }


(* *** ACCESSORIES *** *)

(* these should be light enough that it's not worth making them lazy *)

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
