(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let refpkg_str = "refpkg.txt"

type t = 
  {
    ref_tree    : Newick_bark.newick_bark Gtree.gtree Lazy.t;
    model       : Model.t Lazy.t;
    aln_fasta   : Alignment.t Lazy.t;
    aln_sto     : unit;
    aln_profile : unit;
    taxonomy    : Tax_taxonomy.t Lazy.t;
    seqinfom    : Tax_seqinfo.seqinfo_map Lazy.t;
    timestamp   : string;
  }


(* *** basics *** *)

let get_ref_tree rp =  Lazy.force rp.ref_tree
let get_model rp =     Lazy.force rp.model
let get_aln_fasta rp = Lazy.force rp.aln_fasta
let get_taxonomy rp =  Lazy.force rp.taxonomy
let get_seqinfom rp =  Lazy.force rp.seqinfom

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

(* NOTE: temporary kluge *)
let build_model path stats_fname ref_align = 
  print_endline stats_fname;
  let prefs = Prefs.defaults () in
  prefs.Prefs.stats_fname := stats_fname;
  let ref_dir_complete = path^"/" in
  Model.of_prefs ref_dir_complete prefs ref_align

let of_path path = 
  if not (Sys.is_directory path) then
    failwith ("Purported refpkg "^path^" is not a directory");
  let dirize fname = path^"/"^fname in
  let refpkg_lines = 
    try File_parsing.string_list_of_file (dirize refpkg_str) with
    | Sys_error _ -> invalid_arg (Printf.sprintf "can't find %s in %s" refpkg_str path)
  in
  let eqmap = eqmap_of_strl (File_parsing.filter_comments refpkg_lines) in
  let get what = 
    try StringMap.find what eqmap with
    | Not_found -> invalid_arg (what^" not found in "^(dirize refpkg_str))
  in
  let dget what = dirize (get what) in
  let lazy_fasta_aln = lazy(Alignment.read_fasta(dget "aln_fasta")) in
  {
    ref_tree = lazy (Newick.of_file (dget "tree_file"));
    model    = 
      lazy (build_model path (get "tree_stats") 
                             (Lazy.force lazy_fasta_aln));
    aln_fasta   = lazy_fasta_aln;
    aln_sto     = ();
    aln_profile = ();
    taxonomy    = lazy (Tax_taxonomy.of_ncbi_file (dget "taxonomy"));
    seqinfom    = lazy (Tax_seqinfo.of_csv (dget "seq_info"));
    timestamp   = get "timestamp";
  }
