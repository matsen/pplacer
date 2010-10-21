(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
   For each x we consider the lineage of x, and Fold_left over the taxtree: fun
     (x, anc) -> ListFuns.add_listly (anc, x)
  keep track of the ones without ancestors.
    make sure that there is only one at the end, and keep it.
      Then we List.rev the maps.
*)

open Tax_id
open MapsSets

exception Multiple_roots of tax_id * tax_id
exception No_root

let add_list = List.fold_right TaxIdSet.add 

(* get the most distal taxa in the taxonomy which are represented in til. 
 * preserve the order of til as much as possible *)
let tax_tips_of_tax_list td til = 
  (* s is the set of things we have seen, accu is our list of tips *)
  let rec aux s accu = function
    | x::l -> 
        if TaxIdSet.mem x s then aux s accu l
        else begin
          let lin = Tax_taxonomy.get_lineage td x in
          let removes = TaxIdSetFuns.of_list lin in
          (* below: we add the lineage of our taxonomy
           * and take our ancestors from accu if present *)
          aux
            (add_list lin s) 
            (x::(List.filter (fun x -> not (TaxIdSet.mem x removes)) accu))
            l
        end
    | [] -> accu
  in
  List.rev (aux TaxIdSet.empty [] til)

(* now we build a tree represented by a series of maps from ancestors to
 * descendants *)
let build_topdown_tree td tips = 
  (* rooto is the root of the tree, if its been found *)
  let rec add_ancestry rooto tt ti = 
    if Tax_taxonomy.has_ancestor td ti then begin
      let anc = Tax_taxonomy.get_ancestor td ti in
      let tt' = TaxIdMapFuns.add_listly anc ti tt in
      (* if anc was already in tt then we don't have to add its lineage *)
      if TaxIdMap.mem anc tt then (rooto, tt')
      else add_ancestry rooto tt' anc
    end
    else match rooto with
    | Some root -> 
        if root = ti then (rooto, tt) else raise (Multiple_roots (root, ti))
    | None -> (Some ti, tt)
  in
  let rec aux rooto tt = function
    | ti::l -> 
        assert(not(TaxIdMap.mem ti tt)); (* should be nonredundant list *)
        let (rooto', tt') = add_ancestry rooto tt ti in
        aux rooto' (TaxIdMap.add ti [] tt') l (* add ti itself *)
    | [] -> (rooto, tt)
  in
  match aux None TaxIdMap.empty tips with
  | (Some root, tt) -> (root, TaxIdMap.map List.rev tt)
  | (None, _) -> raise No_root


let stree_and_map_of_topdown_tree root tt = 
  let m = ref IntMap.empty in
  let count = ref (-1) in
  (* side effects heh heh *)
  let add ti = 
    incr count;
    m := IntMap.add (!count) ti !m
  in
  (* note that the order of events below is important *)
  let rec aux ti = 
    match TaxIdMap.find ti tt with
    | [] -> add ti; Stree.Leaf (!count)
    | below -> 
        let tL = List.map aux below in
        add ti; 
        Stree.Node(!count, tL)
  in
  let t = aux root in
  (t, !m)

    (*
  let bl_of_taxid bl_of_rank rank_map =
    let bark_of_taxid bl_of_rank td x =
        new Decor_bark.decor_bark (`Of_bl_name_boot_dlist (Some bl, None, None,
        [taxid]))
*)
