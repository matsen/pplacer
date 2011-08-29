(* pquery stands for placed query.
 *)

open Ppatteries

exception Unplaced_pquery of string list

let sort_placement_list criterion pl =
  List.sort
    ~cmp:(Placement.compare_placements criterion |> flip)
    pl

let rec is_decreasing criterion = function
  | x::y::l ->
      if criterion x >= criterion y then is_decreasing criterion l
      else false
  | _ -> true

type pquery =
  {
    namel      : string list;
    seq        : string;
    place_list : Placement.placement list;
  }

let namel p      = p.namel
let seq p        = p.seq
let place_list p = p.place_list

let multiplicity p = List.length p.namel
let total_multiplicity =
  List.fold_left (fun acc x -> acc + (multiplicity x)) 0

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

let make criterion ~namel ~seq pl =
  {
    namel; seq;
    place_list = sort_placement_list criterion pl
  }

let make_ml_sorted = make Placement.ml_ratio
let make_pp_sorted = make Placement.post_prob

let set_namel pq namel = { pq with namel }

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


