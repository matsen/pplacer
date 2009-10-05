(* pquery.ml
 *
 * pquery stands for placed query, which means that it is a named query sequence
 * with the placement information (which may be empty).
 *)

open MapsSets

let sort_placement_list criterion pl =
  List.sort 
    (fun x y -> - Placement.compare_placements criterion x y) 
    pl

let rec is_decreasing criterion = function
  | x::y::l -> 
      if criterion x >= criterion y then is_decreasing criterion l
      else false
  | _ -> true


type pquery = 
  {
    name       : string;
    seq        : string;
    place_list : Placement.placement list;
  }

let name p       = p.name
let seq p        = p.seq
let place_list p = p.place_list

let opt_best_place pq =
  match place_list pq with
  | best::_ -> Some best
  | [] -> None 

let opt_best_location pq = 
  match opt_best_place pq with
  | Some place -> Some (Placement.location place)
  | None -> None

let is_placed pq = 
  match opt_best_place pq with
  | Some _ -> true
  | None -> false

let best_location pq = 
  match opt_best_location pq with
  | Some loc -> loc
  | None -> failwith "best_location: no location!"

let make criterion ~name ~seq pl = 
  { 
    name = name; 
    seq = seq; 
    place_list = sort_placement_list criterion pl
  }

let make_ml_sorted = make Placement.ml_ratio
let make_pp_sorted = make Placement.post_prob

let sort criterion pq = 
  if is_decreasing criterion (place_list pq) then pq
  else { pq with 
         place_list = sort_placement_list criterion (place_list pq) }
  
let make_map_by_best_loc criterion pquery_list = 
  let (placed_l, unplaced_l) = 
    List.partition is_placed pquery_list in
  (unplaced_l,
    Base.intMap_of_list_listly
      ~key_f:best_location
      ~val_f:(fun x -> x)
      placed_l)
