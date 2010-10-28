(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

exception Missing_element of string

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

(* NOTE: once parsing of stats files is deprecated, we can set the prefs
 * directly, rather than doing this. *)
let build_model stats_fname ref_align = 
  print_endline stats_fname;
  let prefs = Prefs.defaults () in
  prefs.Prefs.stats_fname := stats_fname;
  Model.of_prefs "" prefs ref_align

let of_strmap m = 
  let get what = 
    try StringMap.find what m with
    | Not_found -> raise (Missing_element what)
  in
  (* now we make the inferred things *)
  let lfasta_aln = 
    lazy (Alignment.uppercase (Alignment.read_fasta(get "aln_fasta"))) in
  let lref_tree = lazy (Newick.of_file (get "tree_file")) 
  and lmodel = 
      lazy (build_model (get "tree_stats") (Lazy.force lfasta_aln));
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

let of_path path = of_strmap (Refpkg_parse.strmap_of_path path)


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

(* if the rp is equipped with a taxonomy *)
let tax_equipped rp = 
  try let _ = get_taxonomy rp and _ = get_seqinfom rp in true with
  | Missing_element _ -> false

