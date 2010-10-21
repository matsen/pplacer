(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * taxonomic routines for gtrees.
*)

open Fam_batteries
open MapsSets

(* here we get a map of tip taxonomic annotations *)
let tips_map sim t = 
  IntMap.fold
    (fun k newick_bark m ->
      match newick_bark#get_name_opt with
      | Some name -> IntMap.add k (Tax_seqinfo.tax_id_by_name sim name) m
      | None -> m)
    (Gtree.get_bark_map t)
    IntMap.empty

(* next step is to propogate the taxonomic information up the tree according to
 * common ancestry. *)
let fill_out td t tips_map =
  let m = ref tips_map in
  let _ = 
    Gtree.recur
      (fun id below_tax_ids ->
        let mrca = Tax_taxonomy.list_mrca td below_tax_ids in
        m := IntMap.add id mrca (!m);
        mrca)
      (fun id -> IntMap.find id tips_map)
      t
  in
  !m

(* the next step is to attach names to actual MRCAs in the tree. *)
let mrcam_of_full_map t full_map = 
  let m = ref IntMap.empty in
  let _ = 
    Gtree.recur
      (fun id below ->
        let our_tax_id = IntMap.find id full_map in
        List.iter
          (fun (below_id, below_tax_id) ->
            if our_tax_id <> below_tax_id then
              (* something below is not the same tax_id as us. thus it is an
               * MRCA and we label it as such *)
              m := IntMap.add below_id below_tax_id (!m))
          below;
        (id, our_tax_id))
      (fun id -> (id, IntMap.find id full_map))
      t
  in
  !m

let mrcam_of_data sim td t = 
  mrcam_of_full_map t (fill_out td t (tips_map sim t))
