(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for gtrees which have a tax_bark map.
*)

open Fam_batteries
open MapsSets

type t = Tax_bark.tax_bark Gtree.gtree

(*
let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2
  Gtree.mapi_bark_map (fun _ b -> Tax_bark.of_newick_bark b) t
*)

let get_tax_ido t id = (Gtree.get_bark t id)#get_tax_ido
let get_tax_nameo t id = (Gtree.get_bark t id)#get_tax_nameo

(* here we simply annotate the leaves of the newick tree with taxonomic ids *)
let annotate_newick sim t = 
  let tips_annotated = 
    IntMap.map
      (fun newick_bark ->
        Tax_bark.of_newick_bark
          newick_bark
          (match newick_bark#get_name_opt with
          | Some name -> Some (Tax_seqinfo.tax_id_by_name sim name)
          | None -> None)
          None)
      (Gtree.get_bark_map t)
  in
  Gtree.set_bark_map t tips_annotated

let get_tax_id t id =
  match (Gtree.get_bark t id)#get_tax_ido with
  | Some ti -> ti
  | None -> failwith (Printf.sprintf "node %d lacks taxonomic info" id)

(* next step is to propogate the taxonomic information up the tree according to
 * common ancestry. here t needs taxonomic annotation *)
let mrcaize td t =
  let bmr = ref (Gtree.get_bark_map t) in
  let _ = 
    Gtree.recur
      (fun id below_tax_ids ->
        let mrca = Tax_taxonomy.list_mrca td below_tax_ids in
        bmr := IntMap.add 
                 id ((Gtree.get_bark t id)#set_tax_id mrca) (!bmr);
        mrca)
      (get_tax_id t)
      t
  in
  Gtree.set_bark_map t (!bmr)

(* the next step is to attach names to actual MRCAs in the tree. *)
let mrca_name td t = 
  let bmr = ref (Gtree.get_bark_map t) in
  let _ = 
    Gtree.recur
      (fun id below ->
        let our_tax_id = get_tax_id t id in
        List.iter
          (fun (below_id, below_tax_id) ->
            if our_tax_id <> below_tax_id then
              (* something below is not the same tax_id as us. thus it is an
               * MRCA and we label it as such *)
              bmr := 
                IntMap.add 
                  below_id 
                  ((Gtree.get_bark t below_id)#set_tax_name
                    (Tax_taxonomy.get_tax_name td below_tax_id))
                  (!bmr))
          below;
        (id, our_tax_id))
      (fun id -> (id, get_tax_id t id))
      t
  in
  Gtree.set_bark_map t (!bmr)

let process sim td t = 
  mrca_name td (mrcaize td (annotate_newick sim t))

(* here we are using MRCA name as a proxy for being an MRCA *)
let is_mrca t id = 
  match (Gtree.get_bark t id)#get_tax_nameo with 
  | Some _ -> true 
  | None -> false

let mrca_list t = 
  List.filter
    (is_mrca t)
    (Stree.node_ids (Gtree.get_stree t))

