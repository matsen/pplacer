(* pquery stands for placed query.
 *)

open Ppatteries

exception Unplaced_pquery of string list

let sort_placement_list criterion pl =
  List.sort (comparing criterion |> flip) pl

let rec is_decreasing criterion = function
  | x::y::l ->
      if criterion x >= criterion y then is_decreasing criterion l
      else false
  | _ -> true

exception Name_list_needed

(* namlom is now a list of name, mass pairs *)
type pquery = {
  namlom: (string * float) list;
  seq: string;
  place_list: Placement.placement list;
}
type t = pquery

let seq p = p.seq
let place_list p = p.place_list
let namlom p = p.namlom
let name p =
  match p.namlom with
    | (n, _) :: _ -> n
    | _ -> failwith "no name"
let namel p = List.map fst p.namlom
let force_namel = namel

let multiplicity p = List.map snd p.namlom |> List.fsum

let total_multiplicity =
  List.fold_left (multiplicity |- (+.) |> flip) 0.

let opt_best_something thing criterion pq =
  match place_list pq with
  | h::t ->
      let best = ref h
      and best_val = ref (criterion h) in
      List.iter
        (fun x ->
          let v = criterion h in
          if v > !best_val then begin
            best := x;
            best_val := v;
          end;)
        t;
      Some (thing (!best))
  | [] -> None

let opt_best_place criterion pq =
  opt_best_something (fun p -> p) criterion pq
let opt_best_location criterion pq =
  opt_best_something Placement.location criterion pq

let opt_place_by_location pq loc =
  match List.filter
          (fun p -> loc = Placement.location p)
          (place_list pq) with
  | [] -> None
  | [ x ] -> Some x
  | _ -> failwith "multiple placements in a single location"

let best_something thing criterion pq =
  match opt_best_place criterion pq with
  | Some place -> thing place
  | None -> raise (Unplaced_pquery (namel pq))

let best_place criterion pq =
  best_something (fun p -> p) criterion pq
let best_location criterion pq =
  best_something Placement.location criterion pq

let is_placed pq =
  match place_list pq with
  | [] -> false
  | _ -> true

let make criterion ~namlom ~seq pl =
  {
    seq; namlom;
    place_list = sort_placement_list criterion pl;
  }

let make_ml_sorted = make Placement.ml_ratio
let make_pp_sorted = make Placement.post_prob

let uniform_namel namel = List.map (identity &&& const 1.) namel
let set_mass pq m =
  let names = List.length pq.namlom |> float_of_int in
  { pq with namlom = List.map (second (const (m /. names))) pq.namlom }
let set_namlom pq nm = { pq with namlom = nm }

let apply_to_place_list f pq =
  { pq with place_list = f (pq.place_list) }

let sort criterion pq =
  if is_decreasing criterion (place_list pq) then pq
  else { pq with
         place_list = sort_placement_list criterion (place_list pq) }

let make_map_by_best_loc criterion pquery_list =
  let (placed_l, unplaced_l) =
    List.partition is_placed pquery_list in
  (unplaced_l,
    IntMap.of_f_list_listly
      ~key_f:(best_location criterion)
      ~val_f:(fun x -> x)
      placed_l)

let merge_two pq pq' =
  { pq with namlom = List.append pq.namlom pq'.namlom }
let merge_into pq pql = List.fold_left merge_two pq pql
let merge pql = List.reduce merge_two pql

(* From a translation map produced by Newick_gtree.consolidate and a list of
 * pqueries, renumber and boost the location of each placement in the pqueries
 * according to the translation map. *)
let translate_pql transm pql =
  List.map
    (let open Placement in
     apply_to_place_list
       (List.map
          (fun p ->
            let location, bl_boost = IntMap.find p.location transm in
            {p with location; distal_bl = p.distal_bl +. bl_boost})))
    pql

let renormalize_log_like =
  let open Placement in
  let update getter setter pl =
    pl
    |> List.map (getter &&& identity)
    |> List.partition (fst |- Option.is_some)
    |> Tuple2.mapn
        (List.split
         |- first (List.map Option.get |- ll_normalized_prob)
         |- uncurry (List.map2 setter))
        (List.map snd)
    |> uncurry List.append
  in
  update (ml_ratio |- some) (fun ml_ratio p -> {p with ml_ratio})
  |- update post_prob_opt (fun pp p -> {p with post_prob = Some pp})
  |> apply_to_place_list
