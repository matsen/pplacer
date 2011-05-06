(* This is how we record the most distal placements for a placerun.
 * We store them as a map from the edge numbers where they live to the distance
 * from their position on the edge to the distal side of that edge. We call these (key,value) pairs "marks" on the tree.
 *
 * SPEED: replace sort in the induced maker by something which just takes the
 * smallest. make smallest in fam.batteries?
 *)

open MapsSets
open Fam_batteries

exception Invalid_induced


let fold_bool_or = function
   | x::l -> List.fold_left (||) x l
   | [] -> assert(false)

(* check and make sure there are no extra points on the induced *)
let check_on_stree t ind =
  let rec aux = function
    | Stree.Node(id, tL) ->
        let we_have_one = IntMap.mem id ind in
        if fold_bool_or (List.map aux tL) then begin
          if we_have_one then raise Invalid_induced
          else true
        end
        else we_have_one
    | Stree.Leaf id -> IntMap.mem id ind
  in
  let _ = aux t in ()

let check t ind = check_on_stree (Gtree.get_stree t) ind



(* *** intersection of induceds *** *)
(* surprisingly tricky!
* we just have to put down a mark at the first time we see something
* so if there is a mark in two of the paths below, then we get a mark at the node
* if there is a mark below and at this edge, then we keep this mark
* if there is a mark in both, then we take the min and and give status done
* I know we could use a boolean list for status, but I like this way.
*)

(* status tells us "have we seen a left mark or a right mark yet?" *)
type status = {left : bool; right : bool;}

let status_done = {left=true; right=true;}

let status_or a b =
  {left = a.left || b.left; right = a.right || b.right;}

let fold_status_or = function
   | x::l -> List.fold_left status_or x l
   | [] -> assert(false)

let intersect_on_stree t ind1 ind2 =
  let m = ref IntMap.empty in
  let add k v = m := IntMap.add k v !m in
  (* return if we should continue *)
  let status_add j status =
    if status = status_done then begin
      add j 0.; (* MRCA at node *)
      status_done
    end
    else
      let ofj = IntMap.opt_find j in
      match (ofj ind1, ofj ind2) with
      | (Some p1, Some p2) ->
          add j (max p1 p2); status_done
      (* max so we get the most proximal placement *)
      | (Some p, None) ->
          if status.right then (add j p; status_done)
          else {left=true;right=false}
      | (None, Some p) ->
          if status.left then (add j p; status_done)
          else {left=false;right=true}
      | (None, None) -> status
  in
  let rec aux = function
    | Stree.Node(id, tL) ->
        let below = List.map aux tL in
        if List.mem status_done below then status_done
        else status_add id (fold_status_or below)
    | Stree.Leaf id ->
        status_add id {left=false; right=false}
  in
  let _ = aux t in
  !m

let intersect t ind1 ind2 =
  intersect_on_stree (Gtree.get_stree t) ind1 ind2


(* *** union of induceds *** *)
(* much simpler.
 *)

(* we just have a bool that says if the given edge already has something below
 * it.
 * *)
let union_on_stree t ind1 ind2 =
  let m = ref IntMap.empty in
  let add k v = m := IntMap.add k v !m in
  let attempt_add j =
    let ofj = IntMap.opt_find j in
    match (ofj ind1, ofj ind2) with
    | (Some p1, Some p2) ->
        add j (min p1 p2); true
        (* min so we get the most distal placement *)
    | (Some p, None) -> add j p; true
    | (None, Some p) -> add j p; true
    | (None, None) -> false
  in
  let rec aux = function
    | Stree.Node(id, tL) ->
        if fold_bool_or (List.map aux tL) then true (* something below *)
        else attempt_add id
    | Stree.Leaf id ->
        attempt_add id
  in
  let _ = aux t in
  !m

let union t ind1 ind2 =
  union_on_stree (Gtree.get_stree t) ind1 ind2



(* *** IO *** *)
let of_placerun criterion pr =
  let distal_mark_map =
    IntMap.map
      (List.sort compare)
      (List.fold_right
        (fun pq ->
          let best = Pquery.best_place criterion pq in
          IntMap.add_listly
            (Placement.location best)
            (Placement.distal_bl best))
        (Placerun.get_pqueries pr)
        IntMap.empty)
  in
  let check_edge i =
    if IntMap.mem i distal_mark_map then
      [i, List.hd (IntMap.find i distal_mark_map)]
    else
      []
  in
  IntMap.of_pairlist
    (Stree.recur
      (fun i belowl ->
          let below = List.concat belowl in
          if below = [] then check_edge i
          else below)
      check_edge
      (Gtree.get_stree (Placerun.get_ref_tree pr)))

