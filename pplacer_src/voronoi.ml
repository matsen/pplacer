open MapsSets
open Stree

type leaf = int

type mark = {
  edge_num: int;
  distal_bl: float;
  proximal_leaf: leaf;
}

type ldist = {
  leaf: leaf;
  distance: float;
}

type v = {
  tree: Newick_gtree.t;
  marks: mark list;
  ldistm: ldist IntMap.t;
}

type edge_snip = int * float * float

let mark_min m1 m2 = if (fst m1) <= (fst m2) then m1 else m2

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

let update_ldistm ldistm ?which_leaves gt =
  let leaves = Gtree.leaf_ids gt
  and adjacency_map = adjacent_bls gt in
  let rec aux accum = function
    | [] -> accum
    | n :: rest when List.mem n leaves ->
      aux
        (IntMap.add n {leaf = n; distance = 0.0} accum)
        (List.rev_append (List.map fst (IntMap.find n adjacency_map)) rest)
    | n :: rest ->
      let adj = List.fold_left
        (fun stl (sn, sbl) ->
          match begin
            try
              Some (IntMap.find sn accum)
            with
              | Not_found -> None
          end with
            | Some {leaf = best_leaf; distance = distance} ->
              (sbl +. distance, best_leaf) :: stl
            | None -> stl)
        []
        (IntMap.find n adjacency_map)
      in
      let distance, best_leaf = list_min adj in
      let new_ldist = {leaf = best_leaf; distance = distance} in
      let rest = match begin
        try
          Some (IntMap.find n accum)
        with
          | Not_found -> None
      end with
        | Some ldist when ldist = new_ldist -> rest
        | _ ->
          List.rev_append (List.map fst (IntMap.find n adjacency_map)) rest
      in
      aux
        (IntMap.add n new_ldist accum)
        rest
  in
  aux
    ldistm
    (match which_leaves with
      | None -> leaves
      | Some l -> l)

let ldistm_of_gtree t =
  update_ldistm IntMap.empty t

let of_gtree t =
  let ldistm = ldistm_of_gtree t
  and bl = Gtree.get_bl t in

  let rec aux accum = function
    | [] -> accum
    | Leaf _ :: rest -> aux accum rest
    | Node (n, subtrees) :: rest ->
      let proximal_ldist = IntMap.find n ldistm in
      let accum = List.fold_left
        (fun accum st ->
          let sn = top_id st in
          let distal_ldist = IntMap.find sn ldistm in
          if proximal_ldist = distal_ldist then
            accum
          else
            {
              edge_num = sn;
              proximal_leaf = proximal_ldist.leaf;
              distal_bl = ((bl sn) +. distal_ldist.distance -. proximal_ldist.distance) /. 2.0;
            } :: accum)
        accum
        subtrees
      in
      aux accum (List.rev_append subtrees rest)
  in
  let marks = aux [] [t.Gtree.stree] in

  {tree = t; marks = marks; ldistm = ldistm}
  (* The algorithm is simple: first, with a two pass recursion, find the mark
   * color and distance to the closest leaf for all of the internal nodes. Now
   * say we are on an edge of length l bounded by internal nodes i_d (distal)
   * and i_p (proximal). If the mark color is the same for each of i_d and i_p,
   * then there are no marks inside the given edge. OTOH, if they are different
   * and the distances to the closest leaves are d_d and d_p, respectively, then
   * we should put a mark at (l + d_d - d_p)/2 with color taken from i_d.
 *)

let uncolor_leaf v l = v, [l]
let fold _ _ _ x = x
let get_edge_snipl _ _ = []

