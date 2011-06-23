open Fam_batteries
open MapsSets
open Stree

type leaf = int

type ldist = {
  leaf: leaf;
  distance: float;
}

type ldistm = ldist IntMap.t

type v = {
  tree: Newick_gtree.t;
  ldistm: ldist IntMap.t;
  all_leaves: IntSet.t;
}

type snip = {
  assoc_leaf: int;
  distal_edge: int;
  proximal_edge: int;
  start: float;
  finish: float;
}

type qs = {
  queue: int Queue.t;
  set: IntSet.t;
}

let qs_push {queue = q; set = s} l =
  let s' = List.fold_left
    (fun accum x ->
      if not (IntSet.mem x accum) then
        Queue.push x q;
      IntSet.add x accum)
    s
    l
  in {queue = q; set = s'}

let qs_pop ({queue = q; set = s} as qs) =
  match begin
    try
      Some (Queue.pop q)
    with
      | Queue.Empty -> None
  end with
    | None -> None, qs
    | Some x -> Some x, {qs with set = IntSet.remove x s}

let qs l =
  qs_push {queue = Queue.create (); set = IntSet.empty} l

let list_min ?(key = compare) l =
  match begin
    List.fold_left
      (fun prev cur -> match prev, cur with
        | Some a, b when key a b < 0 -> Some a
        | _, b -> Some b)
      None
      l
  end with
    | Some x -> x
    | None -> invalid_arg "list_min"

(* adjacent_bls produces a map from each id in a Newick_gtree.t to a list of
 * (neighbor_id, distance) pairs for every neighboring node for the given node,
 * where the distance is the edge length between the two nodes. The parent of a
 * node counts as a neighbor.
 *)
let adjacent_bls t =
  let bl = Gtree.get_bl t in
  let rec aux accum = function
    | [] -> accum
    | (above, Leaf n) :: rest ->
      let adj = match above with
        | Some above -> [above, bl n]
        | None -> []
      in
      aux (IntMap.add n adj accum) rest
    | (above, Node (n, subtrees)) :: rest ->
      let adj = List.map
        (fun st -> let sn = top_id st in sn, bl sn)
        subtrees
      in
      let adj = match above with
        | Some above -> (above, bl n) :: adj
        | None -> adj
      in
      aux
        (IntMap.add n adj accum)
        (List.fold_left (fun l sn -> (Some n, sn) :: l) rest subtrees)
  in
  aux IntMap.empty [None, t.Gtree.stree]

let update_ldistm ldistm all_leaves initial_leaves gt =
  let adjacency_map = adjacent_bls gt in
  let concat_adj n qs =
    qs_push qs (List.map fst (IntMap.find n adjacency_map))
  in
  let rec aux ((ldistm', updated_leaves) as accum) rest =
    match qs_pop rest with
      | None, _ -> accum
      | Some n, rest when IntSet.mem n all_leaves ->
        aux
          ((IntMap.add n {leaf = n; distance = 0.0} ldistm'),
           updated_leaves)
          (concat_adj n rest)
      | Some n, rest ->
        let adj = List.fold_left
          (fun stl (sn, sbl) ->
            match begin
              try
                Some (IntMap.find sn ldistm')
              with
                | Not_found -> None
            end with
              | Some {leaf = leaf} when not (IntSet.mem leaf all_leaves) ->
                stl
              | Some {leaf = best_leaf; distance = distance} ->
                (sbl +. distance, best_leaf) :: stl
              | None -> stl)
          []
          (IntMap.find n adjacency_map)
        in
        let ldistm', updated_leaves, rest = match adj with
          | [] -> IntMap.remove n ldistm', updated_leaves, concat_adj n rest
          | adj ->
            let distance, best_leaf = list_min adj in
            let new_ldist = {leaf = best_leaf; distance = distance} in
            let updated_leaves, rest = match begin
              try
                Some (IntMap.find n ldistm')
              with
                | Not_found -> None
            end with
              | None -> updated_leaves, concat_adj n rest
              | Some ldist when ldist = new_ldist -> updated_leaves, rest
              | Some prev_ldist ->
                (if IntSet.mem prev_ldist.leaf all_leaves then
                    IntSet.add prev_ldist.leaf updated_leaves
                 else
                    updated_leaves),
                concat_adj n rest
            in
            IntMap.add n new_ldist ldistm',
            IntSet.add best_leaf updated_leaves,
            rest
        in
        aux (ldistm', updated_leaves) rest
  in
  aux
    (ldistm, IntSet.empty)
    (qs initial_leaves)

let of_gtree t =
  let leaves = Gtree.leaf_ids t in
  let all_leaves = IntSet.of_list leaves in
  let ldistm, _ = update_ldistm IntMap.empty all_leaves leaves t in
  {tree = t; ldistm = ldistm; all_leaves = all_leaves}

let uncolor_leaves v ls =
  let all_leaves' = IntSet.diff v.all_leaves ls in
  let ldistm', updated =
    update_ldistm
      v.ldistm
      all_leaves'
      (IntSet.elements ls)
      v.tree
  in
  {v with all_leaves = all_leaves'; ldistm = ldistm'}, updated

let uncolor_leaf v l =
  uncolor_leaves v (IntSet.singleton l)

let fold f initial {tree = t; ldistm = ldistm} =
  let bl = Gtree.get_bl t in
  let rec aux cur = function
    | [] -> cur
    | Leaf _ :: rest -> aux cur rest
    | Node (n, subtrees) :: rest ->
      let proximal_ldist = IntMap.find n ldistm in
      let cur = List.fold_left
        (fun cur st ->
          let sn = top_id st in
          let distal_ldist = IntMap.find sn ldistm in
          if proximal_ldist.leaf = distal_ldist.leaf then
            f cur
              {assoc_leaf = distal_ldist.leaf;
               distal_edge = sn; proximal_edge = n;
               start = bl sn; finish = 0.0}
          else
            let distal_split =
              ((bl sn) -. distal_ldist.distance +. proximal_ldist.distance) /. 2.0
            in
            let cur = f cur
              {assoc_leaf = proximal_ldist.leaf;
               distal_edge = sn; proximal_edge = n;
               start = bl sn; finish = distal_split}
            in
            let cur = f cur
              {assoc_leaf = distal_ldist.leaf;
               distal_edge = sn; proximal_edge = sn;
               start = distal_split; finish = 0.0}
            in
            cur)
        cur
        subtrees
      in
      aux cur (List.rev_append subtrees rest)
  in
  aux initial [t.Gtree.stree]

let get_edge_snipl v l =
  fold (fun accum snip -> if snip.assoc_leaf = l then snip :: accum else accum) [] v

let get_snipdist v =
  fold
    (fun accum snip ->
      IntMap.add_listly snip.distal_edge snip accum)
    IntMap.empty
    v

let matching_snip snips pos =
  List.find
    (fun {start = st; finish = en} -> st >= pos && pos >= en)
    snips

module I = Mass_map.Indiv
let distribute_mass v mass =
  let snipdist = get_snipdist v in
  IntMap.fold
    (fun n massl accum ->
      let snips = IntMap.find n snipdist in
      List.fold_left
        (fun accum {I.distal_bl = pos; I.mass = mass} ->
          let {assoc_leaf = leaf} = matching_snip snips pos in
          IntMap.add_listly leaf mass accum)
        accum
        massl)
    mass
    IntMap.empty

let placement_distance v ?snipdist p =
  let snipdist = match snipdist with
    | Some m -> m
    | None -> get_snipdist v
  in
  let placement_pos = Placement.distal_bl p in
  let snip = matching_snip
    (IntMap.find (Placement.location p) snipdist)
    placement_pos
  in
  let maybe_min a = function
    | None -> Some a
    | Some b when a < b -> Some a
    | prev -> prev
  in
  let bl = Gtree.get_bl v.tree snip.distal_edge in
  let res = None in
  let res =
    if approx_equal snip.start bl then
      maybe_min
        ((IntMap.find snip.proximal_edge v.ldistm).distance
         +. (snip.start -. placement_pos))
        res
    else
      res
  in
  let res =
    if approx_equal snip.finish 0.0 then
      maybe_min
        ((IntMap.find snip.distal_edge v.ldistm).distance
         +. placement_pos)
        res
    else
      res
  in
  match res with
    | Some d -> d
    | None -> invalid_arg "dist"

module XXX = Placerun_io
module XXY = Convex
