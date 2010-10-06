(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The tax_tree points from a tax_id to its ancestor
 *
 * Note that taxonomic ranks decrease as one goes towards the ancestor.
 * 
 * SPEED: add_lineage_to_tree_and_map could avoid use of pair-redefinitions
 * SPEED: mrca could be made less elegant and faster
*)

open Tax_id
open Fam_batteries

exception NoAncestor of tax_id
exception NoMRCA of tax_id * tax_id

type tax_tree = tax_id TaxIdMap.t
type tax_rank_map = int TaxIdMap.t
type tax_name_map = string TaxIdMap.t

type t = 
  { 
    rank_names      : string array;
    tax_tree        : tax_tree; 
    tax_rank_map    : tax_rank_map;
    tax_name_map    : tax_name_map;
  }

(* basics *)
let get_rank_name td i = 
  try td.rank_names.(i) with 
  | Invalid_argument _ -> invalid_arg "Tax_taxonomy.get_rank_name"

let get_n_ranks td = Array.length td.rank_names

let get_tax_rank td ti = 
  try TaxIdMap.find ti td.tax_rank_map with
  | Not_found -> invalid_arg ("Tax_taxonomy.get_tax_rank not known: "^(Tax_id.to_string ti))

let rank_name_of_tax_id td ti = get_rank_name td (get_tax_rank td ti)

let get_ancestor td ti = 
  try TaxIdMap.find ti td.tax_tree with
  | Not_found -> raise (NoAncestor ti)

let get_tax_name td ti = 
  try TaxIdMap.find ti td.tax_name_map with
  | Not_found -> invalid_arg ("Tax_taxonomy.get_tax_name not known: "^(Tax_id.to_string ti))

(* adds a lineage to the tree and the tax_rank_map *)
let add_lineage_to_tree_and_map (t,m) l = 
  let check_add k v m = 
    try TaxIdMapFuns.check_add k v m with
    | Failure s -> failwith (s^" problem with "^(to_string k))
  in
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
type tax_line = 
  {
    tax_id_str : string;
    parent_id_str : string;
    rank_name : string;
    taxonomic_name : string;
    lineage : string option list;
  }

let tax_line_of_stringol = function 
    | (Some tax_id_str)::(Some parent_id_str)
      ::(Some rank_name)::(Some taxonomic_name)
      ::lineage ->
        {
          tax_id_str = tax_id_str;
          parent_id_str = parent_id_str;
          rank_name = rank_name;
          taxonomic_name = taxonomic_name;
          lineage = lineage;
        }
    | l -> begin
        Format.fprintf 
          Format.std_formatter
          "Error: this tax line didn't fit expectations:@\n%a@\n"
          (Ppr.ppr_list (Ppr.ppr_opt Format.pp_print_string))
          l;
        exit(0);
    end

let of_ncbi_file fname = 
  let taxid_of_stro = ncbi_of_stro 
  and full_list = R_csv.list_list_of_file fname in
  if not (R_csv.list_list_is_rectangular full_list) then
    invalid_arg ("Array not rectangular: "^fname);
  match List.map tax_line_of_stringol full_list with
  | names::lineage_data -> 
      let (tax_tree, tax_rank_map) = 
        List.fold_left
          (fun tam tax_line -> 
            add_lineage_to_tree_and_map tam
              (List.map taxid_of_stro tax_line.lineage))
          (TaxIdMap.empty, TaxIdMap.empty)
          lineage_data
      in
      { 
        rank_names = 
          Array.of_list 
            (List.map 
            (function | Some s -> s 
                      | None -> failwith "NA in taxon rank name line!")
            names.lineage);
        tax_tree = tax_tree;
        tax_rank_map = tax_rank_map;
        tax_name_map = 
          List.fold_right 
            (fun tline -> 
              TaxIdMap.add (taxid_of_stro (Some tline.tax_id_str))
                           tline.taxonomic_name)
            lineage_data
            TaxIdMap.empty
      }
  | _ -> invalid_arg ("empty taxonomy: "^fname)

(* *** writing *** *)
let ppr_tax_tree = TaxIdMapFuns.ppr_gen Tax_id.ppr

let sort_by_rank td ti1 ti2 = 
  let l1 = get_tax_rank td ti1
  and l2 = get_tax_rank td ti2
  in
  if l1 < l2 then (ti1,ti2)
  else (ti2,ti1)

(* *** using *** *)
let rec mrca td ti1 ti2 =
  let rec aux ti1 ti2 = 
    if ti1 = ti2 then ti1
    else 
      let (ti_proximal, ti_distal) = sort_by_rank td ti1 ti2 in
      aux (get_ancestor td ti_distal) ti_proximal
  in
  try aux ti1 ti2 with
  | NoAncestor _ -> raise (NoMRCA (ti1, ti2))

let list_mrca td = function
  | hd::tl -> List.fold_left (mrca td) hd tl
  | [] -> assert(false)
