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

exception Name_list_needed

type namlom =
  | Name_list of string list
  | Named_float of string * float

type pquery = {
  namlom: namlom;
  seq: string;
  place_list: Placement.placement list;
}
type t = pquery

let seq p = p.seq
let place_list p = p.place_list
let namel p =
  match p.namlom with
    | Name_list l -> l
    | _ -> raise Name_list_needed

let multiplicity p =
  match p.namlom with
    | Name_list l -> List.length l |> float_of_int
    | Named_float (_, f) -> f
let naml_multiplicity p =
  match p.namlom with
    | Name_list l -> List.length l
    | _ -> raise Name_list_needed

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

let make criterion ~namel ~seq pl =
  {
    namlom = Name_list namel;
    seq = seq;
    place_list = sort_placement_list criterion pl;
  }

let make_ml_sorted = make Placement.ml_ratio
let make_pp_sorted = make Placement.post_prob

let set_namel pq namel = { pq with namlom = Name_list namel }
let set_mass pq n m = { pq with namlom = Named_float (n, m) }
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


