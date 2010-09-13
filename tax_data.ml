(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The tax_tree points from a tax_id to its ancestor
 *
 * SPEED: add_lineage_to_tree_and_map could avoid use of pair-redefinitions
 * SPEED: mrca could be made less elegant and faster
*)

open Tax_id
open Fam_batteries

exception NoAncestor of tax_id
exception NoMRCA of tax_id * tax_id

type tax_tree = tax_id TaxIdMap.t
type tax_level_map = int TaxIdMap.t

type tax_data = 
  { 
    rank_names      : string array;
    tax_tree        : tax_tree; 
    tax_level_map   : tax_level_map;
  }

(* basics *)
let get_rank_name td i = 
  try td.rank_names.(i) with 
  | Invalid_argument _ -> invalid_arg "Tax_data.get_rank_name"

let get_ancestor td ti = 
  try TaxIdMap.find ti td.tax_tree with
  | Not_found -> raise (NoAncestor ti)

let get_tax_level td ti = 
  try TaxIdMap.find ti td.tax_level_map with
  | Not_found -> invalid_arg ("Tax_data.get_tax_level: "^(Tax_id.to_string ti))

let add_lineage_to_tree_and_map (t,m) l = 
  let check_add = TaxIdMapFuns.check_add in
  let rec aux (t,m) = function
    | (i,x)::((_,y)::_ as l') ->
        aux (check_add y x t, check_add x i m) l'
    | [(i,x)] -> (t, check_add x i m)
    | [] -> (t,m)
  in
  (* filter out the NoTax after adding rank numbers *)
  aux (t,m) (List.filter (fun (_,x) -> x <> NoTax) 
            (ListFuns.mapi (fun i x -> (i,x)) l))

(* *** reading *** *)
let of_ncbi_file fname = 
  match R_csv.list_list_of_file fname with
  | opt_names::indexed_lineage_list as full_list -> 
      if not (R_csv.list_list_is_rectangular full_list) then
        invalid_arg ("Array not rectangular: "^fname);
      let (tax_tree, tax_level_map) = 
        List.fold_left
          (fun t -> function 
            | _::l -> 
                add_lineage_to_tree_and_map t (List.map ncbi_of_stro l)
            | [] -> failwith "empty lineage")
          (TaxIdMap.empty, TaxIdMap.empty)
          indexed_lineage_list
      in
      { 
        rank_names = 
          Array.of_list 
            (List.map 
            (function | Some s -> s 
                      | None -> failwith "NA in taxon level name line!")
            (List.tl opt_names));
        (* above: just take the actual rank names. know nonempty from rectangular & above *)
        tax_tree = tax_tree;
        tax_level_map = tax_level_map;
      }
  | _ -> invalid_arg ("empty taxonomy: "^fname)

(* *** writing *** *)
let ppr_tax_tree = TaxIdMapFuns.ppr_gen Tax_id.ppr

let sort_by_level td ti1 ti2 = 
  let l1 = get_tax_level td ti1
  and l2 = get_tax_level td ti2
  in
  if l1 < l2 then (ti1,ti2)
  else (ti2,ti1)

(* *** using *** *)
let rec mrca td ti1 ti2 =
  let rec aux ti1 ti2 = 
    if ti1 = ti2 then ti1
    else 
      let (ti_proximal, ti_distal) = sort_by_level td ti1 ti2 in
      aux (get_ancestor td ti_distal) ti_proximal
  in
  try aux ti1 ti2 with
  | NoAncestor _ -> raise (NoMRCA (ti1, ti2))

let list_mrca td = function
  | hd::tl -> List.fold_left (mrca td) hd tl
  | [] -> assert(false)
