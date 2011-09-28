(* this code exists to support errplace, and calculates the distance between
 * placements.
*)


open Stree
open Ppatteries


(*
 * we assume that the placement is a leaf, coming off of a bifurcating node
 *)
let find_placement t place_name =
  let combine_results or1 or2 =
    match (or1, or2) with
    | (Some _, Some _) -> failwith "placement placed multiply!"
    | (Some r, None) -> Some r
    | (None, Some r) -> Some r
    | (None, None) -> None
  in
  let is_correct_leaf = function
    | Node(_, _) -> false
    | Leaf i -> place_name = Gtree.get_name t i
  in
  let rec aux = function
    | Node(i, tL) -> begin
        let below_result =
          List.fold_left combine_results None (List.map aux tL) in
        match List.map is_correct_leaf tL with
        | [false; true] ->
    (* the index is two too big because the placement adds two nodes to the clade*)
            combine_results (Some (i-2)) below_result
        | [true; false] ->
            combine_results (Some (i-2)) below_result
        | _ -> below_result
      end
    | Leaf _ -> None
  in
  match aux (Gtree.get_stree t) with
  | Some node_num -> node_num
  | None -> failwith ("find_placement: couldn't find "^place_name)



exception FoundDistance of int

(* edgeDistToNodeDist:
 * go from number of edges along the path to the number of internal nodes *)
let edgeDistToNodeDist nEdges =
  if nEdges = 0 then 0 else nEdges-1

(* calculate the number of nodes of the original tree which must be traversed to
 * get from one edge to another. note that this function assumes that all of the
 * node indices in the tree are distinct.
 * note that this is not the internode distance:
# let s1 = (Stree.of_newick "((x,y),z)").tree;;
val s1 : Stree.stree = ((0,1)2,3)4
# edge_distance s1 0 3;;
- : int = 2
# edge_distance s1 0 4;;
- : int = 2
 * *)
let edge_node_distance stree n1 n2 =
  if n1 = n2 then 0 else begin
    let matches i = n1 = i || n2 = i in
    let foundit d = raise (FoundDistance d) in
    let rec aux = function
      | Node(i, tL) -> begin
        let belows = List.flatten (List.map aux tL) in
        match belows with
        | [] -> if matches i then [0] else []
        | [d] ->
            if matches i then foundit (d+1)
            else [d+1]
        | [d1; d2] ->
            assert(not (matches i)); (* matching should have been done below *)
            foundit (d1+d2+1)
        | _ ->
            failwith "too many places in internode_distance"
      end
    | Leaf i ->
        if matches i then [0] else []
    in
    try
      let _ = aux stree in
      failwith (Printf.sprintf "%d %d: not found in internode_distance" n1 n2)
    with
    | FoundDistance d -> d
  end
