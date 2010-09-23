(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let refpkg_str = "refpkg.txt"

type t = 
  {
    ref_tree    : Newick_bark.newick_bark Gtree.gtree Lazy.t;
    seqinfo     : Tax_seqinfo.seqinfo_map Lazy.t;
    taxonomy    : Tax_taxonomy.t Lazy.t;
    (*
    stats_fname : <the statistics files>
    fasta_aln   : bv_refs_aln.fasta
    sto_aln     : bv_refs.sto
    *)
  }


(* *** basics *** *)

let get_ref_tree rp = Lazy.force rp.ref_tree
let get_seqinfo rp = Lazy.force rp.seqinfo
let get_taxonomy rp = Lazy.force rp.taxonomy

(* *** parsing *** *)

let equality_rex = Str.regexp "[ \t]*=[ \t]*"
let ending_space_rex = Str.regexp "[$[ \t]*]"

let clean_terminal = Str.replace_first ending_space_rex ""

let eqpair_of_str s = 
  match Str.split equality_rex s with
  | [lhs; rhs] -> (lhs, clean_terminal rhs)
  | _ -> failwith ("equality_pair_of_str: malformed string: "^s)

let eqmap_of_strl sl = 
  List.fold_right
    (fun (k,v) -> StringMap.add k v)
    (List.map eqpair_of_str sl)
    StringMap.empty

let of_dname dname = 
  if not (Sys.is_directory dname) then
    failwith ("Purported refpkg "^dname^" is not a directory");
  let dirize fname = dname^"/"^fname in
  let refpkg_lines = 
    try File_parsing.string_list_of_file (dirize refpkg_str) with
    | Sys_error _ -> invalid_arg (Printf.sprintf "can't find %s in %s" refpkg_str dname)
  in
  let eqmap = eqmap_of_strl (File_parsing.filter_comments refpkg_lines) in
  let lazy_get what by = 
    try lazy (by (StringMap.find what eqmap)) with
    | Not_found -> invalid_arg (what^" not found in "^(dirize refpkg_str))
  in
  {
    ref_tree = lazy_get "ref_tree" Newick.of_file;
    seqinfo = lazy_get "seqinfo" Tax_seqinfo.of_csv;
    taxonomy = lazy_get "taxonomy" Tax_taxonomy.of_ncbi_file;
  }
