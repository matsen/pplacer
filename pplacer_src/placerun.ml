(*
 * a placerun is a data structure representing a single pplacer run.
*)

open Ppatteries

type 'a placerun =
  {
    ref_tree  :  'a Gtree.gtree;
    name      :  string;
    pqueries  :  Pquery.pquery list;
  }

let make ref_tree name pqueries =
  {
    ref_tree  =  ref_tree;
    name      =  name;
    pqueries  =  pqueries;
  }

let get_ref_tree p = p.ref_tree
let get_name p = p.name
let get_pqueries p = p.pqueries

let set_ref_tree p ref_tree = {p with ref_tree = ref_tree}
let set_name p name = {p with name = name}
let set_pqueries p pqueries = {p with pqueries = pqueries}

let n_pqueries p = List.length p.pqueries
let total_multiplicity p = Pquery.total_multiplicity p.pqueries

let make_map_by_best_loc criterion pr =
  Pquery.make_map_by_best_loc criterion (get_pqueries pr)

let contains_unplaced_queries p =
  try
    List.iter
      (fun pquery ->
        if not (Pquery.is_placed pquery) then raise Exit)
      (get_pqueries p);
    false
  with
  | Exit -> true

let get_same cmp get_thing thing_name pr1 pr2 =
  let x = get_thing pr1 in
  if 0 = cmp x (get_thing pr2) then x
  else
    failwith
      (Printf.sprintf
        "%ss for %s and %s not the same! Were these run with the same reference tree and model parameters (e.g. statistics files?)"
        thing_name (get_name pr1) (get_name pr2))

let get_same_tree pr1 pr2 =
  get_same Newick_gtree.compare get_ref_tree "Reference tree" pr1 pr2

let combine name pr1 pr2 =
  let ref_tree = get_same_tree pr1 pr2 in
  make
    ref_tree
    name
    ((get_pqueries pr1) @ (get_pqueries pr2))

let warn_about_duplicate_names placerun =
  let _ =
    List.fold_left
      (fun accu name ->
        if StringSet.mem name accu then
          Printf.printf "Warning: query name %s appears multiple times.\n" name;
        StringSet.add name accu)
      StringSet.empty
      (List.flatten
        (List.map Pquery.namel (get_pqueries placerun)))
  in
  ()

let filter_unplaced ?verbose:(verbose=true) pr =
  let (placed_l, unplaced_l) =
    List.partition Pquery.is_placed (get_pqueries pr) in
  if verbose && unplaced_l <> [] then
    Printf.printf "Warning: Ignoring %d unplaced sequences from %s...\n"
      (List.length unplaced_l)
      (get_name pr);
  { pr with pqueries = placed_l }

(* for each entry of a (name, f) list, make a placerun with the given name and
 * the pqueries that satisfy f *)
let multifilter named_f_list placerun =
  let ref_tree = get_ref_tree placerun in
  List.map2
    (make ref_tree)
    (List.map fst named_f_list)
    (ListFuns.multifilter
      (List.map snd named_f_list)
      (get_pqueries placerun))


let cutoff_str x = Printf.sprintf "%1.2g" x
(* let cutoff_str x = Printf.sprintf "%02d" (Base.round (100. *. x)) *)

let cutoff_filter make_name cutoff_fun =
  multifilter
    [ (make_name "lt"), (fun pq -> not (cutoff_fun pq));
      (make_name "ge"), cutoff_fun ]

let re_matches rex s = Str.string_match rex s 0

exception Unimplemented of string

let warn_about_multiple_matches rex_list placerun =
  let (_,_) = (rex_list, placerun) in
  raise (Unimplemented "warn_about_multiple_matches")
  (*
  List.iter
    (fun s ->
      Printf.printf "Warning: multiple match on %s\n" s)
    (Base.find_multiple_matches
      (List.map re_matches rex_list)
      (List.map Pquery.name (get_pqueries placerun)))
*)

let multifilter_by_regex named_regex_list placerun =
  let _ = (placerun,named_regex_list) in
  raise (Unimplemented "multifilter_by_regex")
  (*
  multifilter
    (List.map
      (fun (name, rex) ->
        (name, fun pq -> re_matches rex (Pquery.name pq)))
      named_regex_list)
    placerun
    *)

let redup sequence_tbl pr =
  get_pqueries pr
    |> List.map
        (fun pq ->
          try
            Pquery.namel pq
              |> List.map (Hashtbl.find sequence_tbl)
              |> List.flatten
              |> Pquery.set_namel pq
         with
           | Not_found -> pq)
    |> set_pqueries pr

let transform func pr =
  get_pqueries pr
    |> List.map
        (fun pq -> Pquery.multiplicity pq |> func |> Pquery.set_mass pq)
    |> set_pqueries pr
