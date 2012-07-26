(*
 * a placerun is a data structure representing a single pplacer run.
*)

open Ppatteries

type 'a placerun =
  {
    ref_tree  :  'a Gtree.gtree;
    name      :  string;
    pqueries  :  Pquery.pquery list;
    transm    :  (int * float) IntMap.t option;
  }

type 'a t = 'a placerun

let make ?transm ref_tree name pqueries = {ref_tree; name; pqueries; transm}

let get_ref_tree p = p.ref_tree
let get_name p = p.name
let get_pqueries p = p.pqueries
let get_transm_opt p = p.transm
let get_transm p = Option.get p.transm

let set_ref_tree p ref_tree = {p with ref_tree}
let set_name p name = {p with name}
let set_pqueries p pqueries = {p with pqueries}
let set_transm p tro = {p with transm = Some tro}
let set_transm_opt p transm = {p with transm}

let n_pqueries p = List.length p.pqueries
let total_multiplicity p = Pquery.total_multiplicity p.pqueries

let apply_to_pqueries f p = {p with pqueries = f p.pqueries}
let apply_to_each_placement f =
  List.map f
    |> Pquery.apply_to_place_list
    |> List.map
    |> apply_to_pqueries

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

let redup ?(as_mass = false) sequence_tbl pr =
  let namlom_transform (n, m) =
    if not (Hashtbl.mem sequence_tbl n) then
      [n, m]
    else
      let names = Hashtbl.find_all sequence_tbl n in
      if as_mass then
        [n, List.length names |> float_of_int |> ( *.) m]
      else
        List.map (second (( *.) m)) names
  in
  apply_to_pqueries
    (List.map (fun pq ->
      Pquery.namlom pq
        |> List.map namlom_transform
        |> List.flatten
        |> Pquery.set_namlom pq))
    pr

let transform func pr =
  apply_to_pqueries
    (List.map
       (fun pq -> Pquery.multiplicity pq |> func |> Pquery.set_mass pq))
    pr

let unitize pr =
  let tot_mass = get_pqueries pr
    |> Pquery.total_multiplicity
  in
  apply_to_pqueries
    (List.map
       (fun pq -> Pquery.multiplicity pq /. tot_mass |> Pquery.set_mass pq))
    pr
