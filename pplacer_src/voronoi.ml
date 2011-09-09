open Ppatteries
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
  if IntSet.is_empty all_leaves' then
    failwith "can't remove all leaves from a voronoi graph";
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
let partition_indiv_on_leaves v mass =
  let snipdist = get_snipdist v in
  IntMap.fold
    (fun n massl accum ->
      let snips = IntMap.find n snipdist in
      List.fold_left
        (fun accum ({I.distal_bl = pos} as unit) ->
          let {assoc_leaf = leaf} = matching_snip snips pos in
          IntMap.add
            leaf
            (IntMap.add_listly n unit (IntMap.get leaf IntMap.empty accum))
            accum)
        accum
        massl)
    mass
    IntMap.empty

let distribute_mass v mass =
  IntMap.map
    (fun indiv ->
      IntMap.fold
        (fun _ units accum ->
          List.fold_left
            (fun accum {I.mass = mass} -> mass :: accum)
            accum
            units)
        indiv
        [])
    (partition_indiv_on_leaves v mass)

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


(* voronoi' *)
type mark = float
type solution = {
  leaf_set: IntSet.t;
  mv_dist: float;
  cl_dist: float;
  prox_mass: float;
  wk_subtot: float;
}

let mv_dist {mv_dist} = mv_dist
let leaf_card {leaf_set} = IntSet.cardinal leaf_set

let soln_of_tuple (leaf_set, mv_dist, cl_dist, prox_mass, wk_subtot) =
  {leaf_set; mv_dist; cl_dist; prox_mass; wk_subtot}
let soln_to_tuple {leaf_set; mv_dist; cl_dist; prox_mass; wk_subtot} =
 (leaf_set, mv_dist, cl_dist, prox_mass, wk_subtot)

let print_sol f =
  soln_to_tuple
  |- Tuple5.printn
      (IntSet.print ~first:"{" ~sep:", " ~last:"}" Int.print)
      Float.print
      Float.print
      Float.print
      Float.print
      f

let all_dist_map all_leaves gt =
  let adjacency_map = adjacent_bls gt
  and n_leaves = IntSet.cardinal all_leaves in
  let concat_adj n qs =
    qs_push qs (List.map fst (IntMap.find n adjacency_map))
  in
  let rec aux distm rest =
    match qs_pop rest with
      | None, _ -> distm
      | Some n, rest
        when IntMap.get n IntMap.empty distm |> IntMap.cardinal = n_leaves ->
        aux distm rest
      | Some n, rest ->
        let updated = ref false in
        let distm' = List.fold_left
          (fun m (sn, sbl) ->
            if not (IntMap.mem sn distm) then
              updated := true;
            IntMap.merge
              (fun _ v1 v2 -> match v1, v2 with
                | (Some _) as x, _ -> x
                | None, Some d -> updated := true; Some (d +. sbl)
                | None, None -> None)
              m
              (IntMap.get sn IntMap.empty distm))
          (IntMap.get n IntMap.empty distm)
          (IntMap.find n adjacency_map)
        |> flip (IntMap.add n) distm
        in
        aux
          distm'
          (if !updated then concat_adj n rest else rest)
  in
  aux
    (IntSet.enum all_leaves
      |> Enum.map (identity &&& flip IntMap.singleton 0.0)
      |> IntMap.of_enum)
    (IntSet.elements all_leaves |> qs)

let mark_map gt =
  let distm = all_dist_map (Gtree.leaf_ids gt |> IntSet.of_list) gt
  and parents = Gtree.get_stree gt |> parent_map
  and top = Gtree.top_id gt
  and get_bl = Gtree.get_bl gt in
  let rec aux tree =
    let i, (leaves_below, markm) = match tree with
      | Leaf i -> i, (IntSet.singleton i, IntMap.empty)
      | Node (i, subtrees) ->
        i,
        List.map aux subtrees
          |> List.split
          |> (List.reduce IntSet.union *** List.reduce IntMap.union)
    in
    if i = top then
      leaves_below, markm
    else
      let below = IntMap.find i distm
        |> IntMap.enum
        |> Enum.filter_map
            (fun (k, v) -> if IntSet.mem k leaves_below then Some v else None)
        |> List.of_enum
        |> List.sort_unique compare
      and above = IntMap.find i parents
        |> flip IntMap.find distm
        |> IntMap.enum
        |> Enum.filter_map
            (fun (k, v) -> if IntSet.mem k leaves_below then None else Some v)
        |> List.of_enum
        |> List.sort_unique compare
      and bl = get_bl i in
      leaves_below,
      if abs_float (List.last below -. List.first above) >= bl
        && abs_float (List.first below -. List.last above) >= bl
      then
        markm
      else
        List.enum below
          |> Enum.map
              (fun d ->
                List.enum above
                |> Enum.take_while (fun p -> abs_float (d -. p) < bl)
                |> Enum.map (fun p -> (bl -. d +. p) /. 2.))
          |> Enum.flatten
          |> Enum.filter (not -| approx_equal bl)
          |> List.of_enum
          |> List.sort
          |> flip (IntMap.add i) markm
  in
  Gtree.get_stree gt |> aux |> snd

let does_dominate sup inf =
  sup.mv_dist >= inf.mv_dist
  && sup.cl_dist <= inf.cl_dist
  && sup.wk_subtot <= inf.wk_subtot

let cull sols =
  Printf.printf "culling %d solutions" (List.length sols); flush_all ();
  List.fold_left
    (fun solm sol ->
      let c = IntSet.cardinal sol.leaf_set in
      if IntMap.mem c solm then
        let sols = IntMap.find c solm in
        if List.exists (fun sup -> does_dominate sup sol) sols then
          solm
        else
          sol :: (List.filter (fun inf -> not (does_dominate sol inf)) sols)
          |> flip (IntMap.add c) solm
      else
        IntMap.add c [sol] solm)
    IntMap.empty
    sols
  |> IntMap.values
  |> Enum.map List.enum
  |> Enum.flatten
  |> List.of_enum
  |> tap (fun sols' ->
    let l1 = List.length sols and l2 = List.length sols' in
    if l1 <> l2 then
      Printf.printf " -> culled %d solutions to %d (%g%%; max card %d)\n"
        (l1 - l2)
        l2
        ((float_of_int l1 -. float_of_int l2) /. float_of_int l1 *. 100.)
        (List.enum sols' |> Enum.arg_max leaf_card |> leaf_card)
    else
      print_endline " -> culled nothing")

let arrow_up sol1 sol2 = {
  leaf_set = IntSet.union sol1.leaf_set sol2.leaf_set;
  mv_dist = min sol1.mv_dist sol2.mv_dist;
  cl_dist = min sol1.cl_dist sol2.cl_dist;
  prox_mass = sol1.prox_mass +. sol2.prox_mass;
  wk_subtot = sol1.wk_subtot +. sol2.wk_subtot;
}

let arrow_down sol1 sol2 = {
  leaf_set = IntSet.union sol1.leaf_set sol2.leaf_set;
  mv_dist = infinity;
  cl_dist = min sol1.cl_dist sol2.cl_dist;
  prox_mass = 0.;
  wk_subtot =
    sol1.wk_subtot +. sol2.wk_subtot +. sol2.prox_mass *. sol1.cl_dist;
}

let collapse_marks gt mass markm =
  let get_bl = Gtree.get_bl gt in
  let bubbles_from i =
    List.enum |- flip Enum.append (get_bl i |> Enum.singleton)
  in
  IntMap.mapi
    (fun i marks ->
      let mass_enum = IntMap.get i [] mass |> List.enum in
      Enum.fold
        (fun (delete_prev, accum) mark ->
          if Enum.take_while (fun {I.distal_bl} -> distal_bl < mark) mass_enum
            |> List.of_enum
            |> List.is_empty
          then
            true, mark :: (if delete_prev then List.tl accum else accum)
          else
            false, mark :: accum)
        (false, [])
        (bubbles_from i marks)
      |> snd |> List.tl |> List.rev)
    markm

let combine_solutions max_leaves solsl =
  print_string "combining across ";
  List.print ~first:"" ~last:"" ~sep:", " Int.print stdout (List.map List.length solsl);
  flush_all ();
  List.n_cartesian_product solsl
  |> List.map
      (List.partition (mv_dist |- (=) infinity)
       |- (function
           | i, j when List.is_empty i -> [List.reduce arrow_up j]
           | i, j when List.is_empty j -> [List.reduce arrow_up i]
           | i, j ->
             let i' = List.reduce arrow_up i in
             List.fold_left
               (fun (prev_down, sols) cur ->
                 let cur' = arrow_down prev_down cur in
                 cur',
                 cur' :: List.map (arrow_up cur) sols)
               (i', [i'])
               (List.sort ~cmp:(comparing mv_dist |> flip) j)
             |> uncurry List.cons)
       |- List.filter (leaf_card |- (>=) max_leaves))
  |> List.flatten
  |> tap (fun l -> Printf.printf " -> combined to %d" (List.length l); print_newline ())

let solve gt mass n_leaves =
  let bubbles = mark_map gt |> collapse_marks gt mass
  and get_bl = Gtree.get_bl gt
  and top_id = Gtree.top_id gt in
  let bubbles_of i =
    IntMap.get i [] bubbles
      |> List.enum
      |> flip Enum.append (get_bl i |> Enum.singleton)
  in
  let rec aux tree =
    let i, solutions = match tree with
      | Leaf i -> i,
        [IntSet.empty, 0., infinity, 0., 0.;
         IntSet.singleton i, infinity, 0., 0., 0.]
        |> List.map soln_of_tuple
      | Node (i, subtrees) ->
        i, List.map aux subtrees |> combine_solutions n_leaves |> cull
    in
    if i = top_id then solutions else (* ... *)
    let marks = bubbles_of i
    and masses = IntMap.get i [] mass |> List.enum in
    Enum.fold
      (fun (last_mark, solutions) mark ->
        let masses =
          Enum.take_while (fun {I.distal_bl} -> distal_bl < mark) masses
          |> List.of_enum
        and bub_len = mark -. last_mark in
        let bub_mass = I.v_mass masses
        and wk_distal = I.work_moving_to masses last_mark
        and wk_prox = I.work_moving_to masses mark in
        Printf.printf "%d: %g (%g) -> %g %g %g %g" i mark last_mark bub_len bub_mass wk_distal wk_prox;
        flush_all ();
        mark,
        List.fold_left
          (fun accum sol ->
            let accum = accum
              |> maybe_cons
                  (if bub_mass > 0.
                      && wk_prox < wk_distal +. bub_mass *. sol.cl_dist
                      && sol.cl_dist <> infinity
                   then Some {sol with
                     mv_dist = sol.cl_dist +. ((wk_distal -. wk_prox) /. bub_mass);
                     cl_dist = sol.cl_dist +. bub_len;
                     prox_mass = bub_mass;
                     wk_subtot = sol.wk_subtot +. wk_prox;
                   }
                   else None)
            in
            match sol with
              | sol when sol.mv_dist = 0. ->
                {sol with
                  prox_mass = sol.prox_mass +. bub_mass;
                  wk_subtot = sol.wk_subtot +. wk_prox +. sol.prox_mass *. bub_len}
                :: accum
              | sol when sol.mv_dist < bub_len ->
                accum
              | sol when sol.mv_dist = infinity ->
                {sol with
                  cl_dist = sol.cl_dist +. bub_len;
                  wk_subtot = sol.wk_subtot +. wk_distal}
                :: accum
              | sol ->
                {sol with
                  mv_dist = sol.mv_dist -. bub_len;
                  cl_dist = sol.cl_dist +. bub_len;
                  prox_mass = sol.prox_mass +. bub_mass;
                  wk_subtot = sol.wk_subtot +. wk_prox +. sol.prox_mass *. bub_len}
                :: accum)
          []
          solutions
        |> tap (fun _ -> print_endline " -> finished")
        |> if bub_mass > 0. then cull else identity)
      (0., solutions)
      marks
    |> snd
  in
  Gtree.get_stree gt |> aux
