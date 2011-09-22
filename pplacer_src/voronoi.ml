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

let of_gtree_and_leaves tree all_leaves =
  let ldistm, _ = update_ldistm
    IntMap.empty
    all_leaves
    (IntSet.elements all_leaves)
    tree
  in
  {tree; ldistm; all_leaves}

let of_gtree tree =
  Gtree.leaf_ids tree
    |> IntSet.of_list
    |> of_gtree_and_leaves tree

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

let leaf_work ?(p_exp = 1.) v indiv_map leaf =
  if not (IntMap.mem leaf indiv_map) then 0.0 else
    let indiv = IntMap.find leaf indiv_map in
    let squashed_indiv = IntMap.singleton
      leaf
      [{I.distal_bl = 0.0; I.mass = I.total_mass indiv}]
    in
    Kr_distance.dist v.tree p_exp indiv squashed_indiv

let ecld ?p_exp v indiv_map =
  IntSet.fold
    (leaf_work ?p_exp v indiv_map |- (+.))
    v.all_leaves
    0.


(* voronoi' *)
type partial_solution = {
  leaf_set: IntSet.t;
  mv_dist: float;
  cl_dist: float;
  prox_mass: float;
  wk_subtot: float;
}

type solution = {
  leaves: IntSet.t;
  work: float;
}
type solutions = solution IntMap.t

let mv_dist {mv_dist} = mv_dist
let leaf_card {leaf_set} = IntSet.cardinal leaf_set

let soln_of_tuple (leaf_set, mv_dist, cl_dist, prox_mass, wk_subtot) =
  {leaf_set; mv_dist; cl_dist; prox_mass; wk_subtot}
let soln_to_tuple {leaf_set; mv_dist; cl_dist; prox_mass; wk_subtot} =
 (leaf_set, mv_dist, cl_dist, prox_mass, wk_subtot)

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
  and get_bl = Gtree.get_bl gt
  and st = Gtree.get_stree gt in
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
      (* XXX revisit how to use take_while instead of filter *)
      List.enum above
        |> Enum.map
            (fun p ->
              List.enum below
              |> Enum.filter (fun d -> abs_float (d -. p) < bl)
              |> Enum.map (fun d -> (bl -. d +. p) /. 2.))
        |> Enum.flatten
        |> Enum.filter (not -| approx_equal bl)
        |> List.of_enum
        |> List.sort_unique compare
        |> flip (IntMap.add i) markm
  in
  Gtree.get_stree gt |> aux |> snd,
  IntMap.mapi
    (fun i m -> IntMap.enum m
     |> Enum.filter
         (Stree.find i st
          |> Stree.leaf_ids
          |> IntSet.of_list
          |> flip (IntSet.mem |-- not)
          |~ fst)
     |> Enum.fold (snd |- min |> flip) infinity)
    distm

let does_dominate sup inf =
  sup.cl_dist <= inf.cl_dist
  && sup.prox_mass <= inf.prox_mass
  && sup.wk_subtot <= inf.wk_subtot

let empty_pairmap = Tuple2.compare ~cmp1:(-) ~cmp2:Bool.compare |> Map.create
let cull ?(verbose = false) sols =
  if verbose then begin
    Printf.eprintf "culling solutions";
    flush_all ()
  end;
  let count = ref 0 in
  Enum.fold
    (fun solm sol ->
      incr count;
      let key = IntSet.cardinal sol.leaf_set, sol.mv_dist = infinity in
      if Map.mem key solm then
        let sols = Map.find key solm in
        if List.exists (fun sup -> does_dominate sup sol) sols then
          solm
        else
          sol :: (List.filter (fun inf -> not (does_dominate sol inf)) sols)
          |> flip (Map.add key) solm
      else
        Map.add key [sol] solm)
    empty_pairmap
    sols
  |> Map.values
  |> Enum.map List.enum
  |> Enum.flatten
  |> List.of_enum
  |> if verbose then tap
      (fun sols' ->
        let l1 = !count and l2 = List.length sols' in
        if l1 <> l2 then
          Printf.eprintf
            " -> culled %d solutions from %d to %d (%g%%; max card %d)\n"
            (l1 - l2)
            l1
            l2
            ((float_of_int l1 -. float_of_int l2) /. float_of_int l1 *. 100.)
            (List.enum sols' |> Enum.arg_max leaf_card |> leaf_card)
        else
          Printf.eprintf " -> culled nothing")
    else identity

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

let quiet_product l =
  try
    List.n_cartesian_product l
  with Stack_overflow -> failwith "product too big"

let combine_solutions ?(verbose = false) max_leaves _ solsl =
  if verbose then begin
    Printf.eprintf "combining across ";
    List.print ~first:"" ~last:"; " ~sep:", "
      Int.print stderr (List.map List.length solsl);
    flush_all ()
  end;
  solsl
  |> quiet_product
  |> List.enum
  |> Enum.map
      (List.partition (mv_dist |- (=) infinity)
       |- (function
           | [i], [j] -> [arrow_up i j; arrow_down i j]
           | [], [a; b]
           | [a; b], [] -> [arrow_up a b]
           | [], l
           | l, [] -> [List.reduce arrow_up l]
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
  |> Enum.map List.enum
  |> Enum.flatten
  |> if verbose then
      Enum.suffix_action (fun () -> Printf.eprintf " (finished combining)"; flush_all ())
    else identity

let solve ?(verbose = false) gt mass n_leaves =
  let markm, cleafm = mark_map gt
  and mass = I.sort mass in
  let bubbles = collapse_marks gt mass markm
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
        let closest_leaf = IntMap.find i cleafm in
        i,
        List.map aux subtrees
          |> combine_solutions ~verbose n_leaves closest_leaf
          |> cull ~verbose

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
        if verbose then begin
          Printf.eprintf "%d: %g (%g) -> %g %g %g %g"
            i mark last_mark bub_len bub_mass wk_distal wk_prox;
          flush_all ()
        end;
        mark,
        List.fold_left
          (fun accum sol ->
            let accum = accum
              |> maybe_cons
                  (if bub_mass > 0.
                      && sol.cl_dist <> infinity
                      && sol.mv_dist = infinity
                      && wk_prox < wk_distal +. bub_mass *. sol.cl_dist
                   then Some {sol with
                     mv_dist = sol.cl_dist +. ((wk_distal -. wk_prox) /. bub_mass);
                     cl_dist = sol.cl_dist +. bub_len;
                     prox_mass = bub_mass;
                     wk_subtot = sol.wk_subtot +. wk_prox;
                   }
                   else None)
            in
            match sol with
              | sol when approx_equal sol.mv_dist 0. ->
                {sol with
                  prox_mass = sol.prox_mass +. bub_mass;
                  wk_subtot = sol.wk_subtot +. wk_prox +. sol.prox_mass *. bub_len;
                  cl_dist = sol.cl_dist +. bub_len}
                :: accum
              | sol when sol.mv_dist < bub_len ->
                accum
              | sol when sol.mv_dist = infinity ->
                {sol with
                  cl_dist = sol.cl_dist +. bub_len;
                  wk_subtot = sol.wk_subtot +. wk_distal +. bub_mass *. sol.cl_dist}
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
        |> (if verbose then tap (fun _ -> Printf.eprintf " -> finished\n") else identity)
        |> (if bub_mass > 0. then List.enum |- (cull ~verbose) else identity))
      (0., solutions)
      marks
    |> snd

  in
  Gtree.get_stree gt |> aux

let rec powerset = function
  | [] -> [[]]
  | hd :: tl ->
    List.fold_left (fun accum x -> (hd :: x) :: x :: accum) [] (powerset tl)

let force gt mass ?(strict = true) ?verbose:_ n_leaves =
  let leaves_ecld leaves =
    let v = of_gtree_and_leaves gt leaves in
    partition_indiv_on_leaves v mass |> ecld v
  in
  Gtree.leaf_ids gt
    |> powerset
    |> List.filter_map
        (function
          | [] -> None
          | l when strict && List.length l <> n_leaves -> None
          | l when List.length l <= n_leaves -> Some (IntSet.of_list l)
          | _ -> None)
    |> List.group (comparing IntSet.cardinal)
    |> List.map
        (List.enum |- Enum.map (identity &&& leaves_ecld) |- Enum.arg_min snd)
    |> List.enum
    |> Enum.map (fun (leaves, work) -> IntSet.cardinal leaves, {leaves; work})
    |> IntMap.of_enum

module type Alg = sig
  val solve:
    Newick_gtree.t -> Mass_map.Indiv.t -> ?strict:bool -> ?verbose:bool -> int -> solutions
end

module Full = struct
  let solve gt mass ?strict:_ ?verbose n_leaves =
    solve ?verbose gt mass n_leaves
      |> List.enum
      |> Enum.filter (fun {mv_dist} -> mv_dist = infinity)
      |> Enum.group leaf_card
      |> Enum.map (Enum.arg_min (fun {wk_subtot} -> wk_subtot))
      |> Enum.map
          (fun {leaf_set; wk_subtot} ->
            IntSet.cardinal leaf_set,
            {leaves = leaf_set; work = wk_subtot})
      |> IntMap.of_enum

end

module Forced = struct
  let solve = force
end

let update_score indiv v leaf map =
  let v', _ = uncolor_leaf v leaf in
  ecld v' (partition_indiv_on_leaves v' indiv)
  |> flip (IntMap.add leaf) map

module Greedy = struct
  let solve gt mass ?strict:_ ?(verbose = false) n_leaves =
    let rec aux diagram accum score_map updated_leaves =
      if IntSet.cardinal diagram.all_leaves <= n_leaves then
        accum
      else (* ... *)
      let score_map' = IntSet.fold
        (update_score mass diagram)
        updated_leaves
        score_map
      in
      let leaf, work = IntMap.enum score_map' |> Enum.arg_min snd in
      if verbose then
        Printf.eprintf "uncoloring %d (score %g)\n" leaf work;
      let diagram', updated_leaves' = uncolor_leaf diagram leaf in
      let accum' =
        IntMap.add
          (IntSet.cardinal diagram'.all_leaves)
          {work; leaves = diagram'.all_leaves}
          accum
      in
      aux
        diagram'
        accum'
        (IntMap.remove leaf score_map')
        (IntSet.remove leaf updated_leaves')
    in
    let v = of_gtree gt in
    aux v IntMap.empty IntMap.empty v.all_leaves

end
