(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for gtrees which have a tax_bark map.
*)

open Fam_batteries
open MapsSets

(*
let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2
  Gtree.mapi_bark_map (fun _ b -> Tax_bark.of_newick_bark b) t
*)

(* here we simply annotate the leaves of the newick tree with taxonomic ids *)
let annotate_newick t sim = 
  let tips_annotated = 
    IntMap.map
      (fun newick_bark ->
        Tax_bark.of_newick_bark
          newick_bark
          (match newick_bark#get_name_opt with
          | Some name -> Some (Tax_seqinfo.tax_id_by_name sim name)
          | None -> None))
      (Gtree.get_bark_map t)
  in
  Gtree.set_bark_map t tips_annotated

(* next step is to propogate the taxonomic information up the tree according to
 * common ancestry. here t needs taxonomic annotation *)
let mrcaize t td =
  let st = Gtree.get_stree t
  and bmr = ref (Gtree.get_bark_map t) in
  let _ = 
    Stree.recur
      (fun id below_tax_ids ->
        let mrca = Tax_taxonomy.list_mrca td below_tax_ids in
        bmr := IntMap.add 
                 id ((Gtree.get_bark t id)#set_tax_id mrca) (!bmr);
        mrca)
      (fun id ->
        match (Gtree.get_bark t id)#get_tax_ido with
        | Some ti -> ti
        | None -> failwith (Printf.sprintf "leaf %d lacks taxonomic info" id))
      st
  in
  Gtree.set_bark_map t (!bmr)

(* we call this the detailed version because it includes taxonomic names and
 * ranks, in contrast to the default object bark version, which only writes out
 * the tax ids. *)
let write_detailed_xml td ?name ch t = 
  Phyloxml.write_tree_gen 
    (fun ch b ->
      b#write_xml;
      match b#get_tax_ido with
      | Some tax_id -> begin
        Xml.write_tag 
          output_string
          "scientific_name" 
          ch
          (Tax_taxonomy.get_tax_name td tax_id);
        Xml.write_tag 
          output_string
          "rank" 
          ch
          (Tax_taxonomy.rank_name_of_tax_id td tax_id)
      end
      | None -> ())
    ?name ch t
