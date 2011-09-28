(* Calculating the barycenter of a mass distribution on a tree.
*)

open Ppatteries
open Mass_map

(* masses *)
let list_sum = List.fold_left (fun accu x -> accu +. x) 0.

(* get the sub-map with just the keys in key_list. *)
let submap m key_list =
  List.fold_right
    (fun k accu ->
      if IntMap.mem k m then
        IntMap.add k (IntMap.find k m) accu
      else
        accu)
    key_list
    IntMap.empty

let singleton_map k v = IntMap.add k v IntMap.empty

(* Count the numer of elements of the given list which evaluate to true upon the
 * application of f. *)
let list_count f l =
  List.fold_left (fun accu x -> if f x then accu+1 else accu) 0 l

(*
 * Collect all of the ids which are distal to a given internal node.
 * Note that "wanted" is not included.
 * Note: just returns the empty list if the id is not in the tree.
 *
# let t = Gtree.get_stree (Newick_gtree.of_string "((a,b),(c,d))");;
val t : Stree.stree = ((0,1)2,(3,4)5)6
# let ids = Barycenter.collect_distal_ids t 2;;
val ids : int list = [0; 1]
# let ids = Barycenter.collect_proximal_ids t 2;;
val ids : int list = [6; 5; 3; 4]
*
*)
let collect_distal_ids stree wanted =
  let rec aux = function
    | Stree.Node(i, tL) ->
        if i = wanted then
          List.flatten (List.map Stree.node_ids tL)
        else
          (let below = List.map aux tL in
(* make sure we don't have the id appearing multiple places *)
          assert(list_count (( <> ) []) below <= 1);
          List.flatten below)
(* if we get to a leaf but haven't hit wanted yet then the leaf must be wanted.
 * however, in that case we don't include it by the above comment *)
    | Stree.Leaf _ -> []
  in
  aux stree

(*
 * This time, collect the proximal ids.
 * We go down the tree, and stop until we hit wanted.
 * Note: returns all of the ids in the tree if the id is not in the tree.
 *)
let collect_proximal_ids stree wanted =
  let rec aux = function
    | Stree.Node(i, tL) ->
        if i = wanted then [] (* stop *)
        else (i :: (List.flatten (List.map aux tL)))
    | Stree.Leaf i -> if i = wanted then [] else [i]
  in
  aux stree

exception Found_edge of int
exception Found_node of int
type action = Continue | Stop

(* Find the barycenter given a ref tree and an Indiv mass map.
 * Not especially optimized, as it makes submaps for each work calculation.
 * Pos is the position along the edge, which is a distal_bl.
 *)
module I = Mass_map.Indiv
let find ref_tree mass_m =
  let smass = Indiv.sort mass_m in
  (* The work to move the mass in sub_mass to a point mass on edge id at the
   * given position. *)
  let work sub_mass id pos =
    Kr_distance.dist ref_tree 1.
      sub_mass
      (singleton_map id [{I.distal_bl = pos; I.mass = I.total_mass sub_mass}])
  in
  (* The amount of work required to move all of the mass on the chosen side, as
   * well as the edge_mass, to pos on id. The chosen side is determined by
   * collect_fun which is either collect_distal_ids or collect_proximal_ids.
   * Edge mass is simply a list of Indiv mass units that sit on the chosen side
   * of the chosen point. *)
  let tree_work collect_fun edge_mass id pos =
    let sub_mass =
      IntMap.add id edge_mass
          (submap smass (collect_fun (Gtree.get_stree ref_tree) id))
    in
    work sub_mass id pos
  in
  (* ml stands for mass list.
   * prox_ml is extra mass that is thought of as living on the proximal side of
   * the edge, i.e. the edge_mass as described above. *)
  let proximal_work prox_ml id pos =
    List.iter (fun {I.distal_bl = m_pos} -> assert(m_pos >= pos)) prox_ml;
    tree_work collect_proximal_ids prox_ml id pos
  and distal_work dist_ml id pos =
    List.iter (fun {I.distal_bl = m_pos} -> assert(m_pos <= pos)) dist_ml;
    tree_work collect_distal_ids dist_ml id pos
  in
  (* The difference between the proximal and distal work. *)
  let delta ~prox_ml ~dist_ml id pos =
    (proximal_work prox_ml id pos) -. (distal_work dist_ml id pos)
  in
  let get_mass_list id =
    if IntMap.mem id smass then IntMap.find id smass
    else []
  in
  (* Check determines if the barycenter-containing edge is below the given id,
   * by telling us to Continue (it's below), Stop (it's above), or it raises a
   * Found_edge, which means it's on edge id. *)
  let check id =
    let bl = Gtree.get_bl ref_tree id
    and our_mass_list = get_mass_list id in
    if 0. > delta ~prox_ml:our_mass_list ~dist_ml:[] id 0.
    (* We are negative at the bottom of the edge. continue. *)
    then Continue
    else if 0. > delta ~prox_ml:[] ~dist_ml:our_mass_list id bl
    (* Top is negative (and bottom is positive from before) *)
    then raise (Found_edge id)
    else
    (* Top is positive *)
      Stop
  in
  (* Find the edge or node where the barycenter lies. *)
  let rec find_loc = function
    | Stree.Leaf id -> check id
    | Stree.Node(id, tL) ->
        (match check id with
        | Continue ->
            let below = List.map find_loc tL in
  (* This edge is negative at the bottom but nothing was found.
   * The deepest node to have this must have positive at the tops of all of the
   * edges. We assert to make sure this is the case. *)
            List.iter (fun b -> assert(b = Stop)) below;
            raise (Found_node id)
        | Stop as s -> s)
  in
  (* da and dc are Delta(a) and Delta(c) in the barycenter scan. *)
  let find_pos id =
    let bl = Gtree.get_bl ref_tree id in
    let rec aux ~dist_ml ~prox_ml curr_pos =
      let our_delta pos =
        assert(pos <= bl);
        delta ~dist_ml ~prox_ml id pos
      in
    (* The barycenter formula. Because da is negative we are essentially taking
     * a weighted average here. *)
      let bary ~above_pos da =
        assert(da <= 0.);
        let dc = our_delta curr_pos in
        assert(dc >= 0.);
        curr_pos +. (above_pos -. curr_pos) *. dc /. (dc -. da)
      in
      match prox_ml with
      | [] -> (* at the top of the edge *)
          bary ~above_pos:bl (our_delta bl)
      | {I.distal_bl = pos} as v :: rest -> begin
        (* pos is now the next position up *)
        let da = our_delta pos in
        if da > 0. then
          (* we can do better by moving past this placement *)
          aux
            ~dist_ml:(v::dist_ml)
            ~prox_ml:rest
            pos
        else
          bary ~above_pos:pos da
      end
    in
    (* start at the bottom with all the placements on top *)
    aux ~dist_ml:[] ~prox_ml:(get_mass_list id) 0.
  in
  try
    let _ = find_loc (Gtree.get_stree ref_tree) in
    failwith "failed to find barycenter edge/node!"
  with
  | Found_node id ->
      (* the node is at the bottom of the edge *)
      (id, 0.)
  | Found_edge id ->
      (id, find_pos id)

(* pre means Mass_map.Pre *)
let of_pre t pmm = find t (Indiv.of_pre pmm)

let of_placerun weighting criterion pr =
  find
    (Placerun.get_ref_tree pr)
    (Indiv.of_placerun
      weighting
      criterion
      pr)
