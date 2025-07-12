(* The tax_tree points from a tax_id to its ancestor.
 *
 * Note that taxonomic ranks decrease as one goes towards the ancestor.
 *
 * SPEED: add_lineage_to_tree_and_map could avoid use of pair-redefinitions
 * SPEED: mrca could be made less elegant and faster
*)

open Tax_id
open Ppatteries

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

let get_rank_index td rk =
  Array.findi ((=) rk) td.rank_names

let get_n_ranks td = Array.length td.rank_names

let get_tax_rank td ti =
  try TaxIdMap.find ti td.tax_rank_map with
  | Not_found -> invalid_arg ("Tax_taxonomy.get_tax_rank not known: "^(Tax_id.to_string ti))

let rank_name_of_tax_id td ti = get_rank_name td (get_tax_rank td ti)

let get_ancestor td ti =
  try TaxIdMap.find ti td.tax_tree with
  | Not_found -> raise (NoAncestor ti)

let get_ancestor_opt td ti =
  TaxIdMap.Exceptionless.find ti td.tax_tree

let get_tax_name td ti =
  try TaxIdMap.find ti td.tax_name_map with
  | Not_found -> invalid_arg ("Tax_taxonomy.get_tax_name not known: "^(Tax_id.to_string ti))

let get_lineage td = function
  | NoTax -> []
  | TaxStr _ as ti -> (* ... *)
  let rec aux accu ti' =
    let accu' = ti' :: accu in
    match get_ancestor_opt td ti' with
      | Some ancestor -> aux accu' ancestor
      | None -> accu'
  in
  aux [] ti

(* adds a lineage to the tree and the tax_rank_map *)
let add_lineage_to_tree_and_map (t,m) l =
  let check_add format_error k v m =
    match begin
      try
        Some (TaxIdMap.find k m)
      with Not_found -> None
    end with
      | Some v' when v <> v' -> failwith (format_error k v v')
      | Some _ -> m
      | None -> TaxIdMap.add k v m
  in
  let check_add_tid = check_add
    (fun k v v' ->
      Printf.sprintf
        "Tax table broken: tax_id %s had established parent %s but %s is \
         claiming to be the parent."
        (to_string k)
        (to_string v')
        (to_string v))
  and check_add_rank = check_add
    (fun k v v' ->
      Printf.sprintf
        "Tax table broken: %s had established rank %d but was also found at \
         rank %d."
        (to_string k)
        v'
        v)
  in
  let rec aux (t,m) = function
    | (i,x)::((_,y)::_ as l') ->
      aux (check_add_tid y x t, check_add_rank x i m) l'
    | [(i,x)] -> (t, check_add_rank x i m)
    | [] -> (t,m)
  in
  (* filter out the NoTax after adding rank numbers *)
  aux (t,m) (List.filter (fun (_,x) -> x <> NoTax)
            (List.mapi (fun i x -> (i,x)) l))

(* *** reading *** *)

type tax_line =
  {
    tax_id_str : string;
    parent_id_str : string;
    rank_name : string;
    taxonomic_name : string;
    lineage : string option list;
  }

let tax_line_of_strol = function
    | (Some tax_id_str)::(Some parent_id_str)
      ::(Some rank_name)::(Some taxonomic_name)
      ::lineage ->
        {
          tax_id_str; parent_id_str; rank_name; taxonomic_name; lineage;
        }
    | l -> begin
        Format.fprintf
          Format.std_formatter
          "Error: this tax line didn't fit expectations:@\n%a@\n"
          (Ppr.ppr_list (Ppr.ppr_opt Format.pp_print_string))
          l;
        exit(0);
    end

(* if a list list is rectangular *)
let list_list_is_rectangular = function
  | x::l -> begin
      try
        let x_l = List.length x in
        List.iter
          (fun y -> if x_l <> List.length y then raise Exit)
          l;
        true
      with
      | Exit -> false
    end
  | [] -> true

let of_ncbi_file csv =
  let taxid_of_stro = of_stro
  and full_list =
    List.map (List.map Tax_seqinfo.entry_of_str) (Csv.input_all csv) in
  if not (list_list_is_rectangular full_list) then
    invalid_arg "Taxonomy array not rectangular";
  match List.map tax_line_of_strol full_list with
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
        tax_tree;
        rank_names =
          Array.of_list
            (List.map
            (function | Some s -> s
                      | None -> failwith "NA in taxon rank name line!")
            names.lineage);
        tax_rank_map = TaxIdMap.add NoTax 0 tax_rank_map;
        tax_name_map =
          List.fold_right
            (fun tline ->
              TaxIdMap.add (taxid_of_stro (Some tline.tax_id_str))
                           tline.taxonomic_name)
            lineage_data
            TaxIdMap.empty
      }
  | _ -> invalid_arg "empty taxonomy"

(* *** writing *** *)
let ppr_tax_tree = TaxIdMap.ppr_gen Tax_id.ppr

let sort_by_rank td ti1 ti2 =
  let l1 = get_tax_rank td ti1
  and l2 = get_tax_rank td ti2
  in
  if l1 < l2 then (ti1,ti2)
  else (ti2,ti1)

(* *** using *** *)
let mrca td ti1 ti2 =
  let rec aux = function
    | x, NoTax
    | NoTax, x -> x
    | ti1, ti2 when ti1 = ti2 -> ti1
    | ti1, ti2 ->
      let (ti_proximal, ti_distal) = sort_by_rank td ti1 ti2 in
      aux (get_ancestor td ti_distal, ti_proximal)
  in
  try aux (ti1, ti2) with
  | NoAncestor _ -> raise (NoMRCA (ti1, ti2))

let list_mrca td = function
  | hd::tl -> List.fold_left (mrca td) hd tl
  | [] -> invalid_arg "list_mrca"

(* unused and untested *)
let count_entries_by_rank td =
  let a = Array.make (get_n_ranks td) 0 in
  TaxIdMap.iter
    (fun ti _ ->
      let rk = get_tax_rank td ti in
      a.(rk) <- a.(rk) + 1)
    td.tax_name_map;
  a
