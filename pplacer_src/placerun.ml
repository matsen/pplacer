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
          dprintf "Warning: query name %s appears multiple times.\n" name;
        StringSet.add name accu)
      StringSet.empty
      (List.flatten
        (List.map Pquery.namel (get_pqueries placerun)))
  in
  ()

let filter_unplaced pr =
  let (placed_l, unplaced_l) =
    List.partition Pquery.is_placed (get_pqueries pr) in
  if unplaced_l <> [] then
    dprintf "Warning: Ignoring %d unplaced sequences from %s...\n"
      (List.length unplaced_l)
      (get_name pr);
  { pr with pqueries = placed_l }

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
