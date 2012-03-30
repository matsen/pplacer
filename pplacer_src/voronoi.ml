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

(* qs: queue and set. An item will only be enqueued if it's not already present
 * in the set. *)
type qs = {
  queue: int Queue.t;
  set: IntSet.t;
}

(* push an item on the queue if it's not in the set. *)
let qs_push {queue = q; set = s} l =
  let s' = List.fold_left
    (fun accum x ->
      if not (IntSet.mem x accum) then
        Queue.push x q;
      IntSet.add x accum)
    s
    l
  in {queue = q; set = s'}

(* pop an item from the queue; remove it from the set. *)
let qs_pop ({queue = q; set = s} as qs) =
  match begin
    try
      Some (Queue.pop q)
    with
      | Queue.Empty -> None
  end with
    | None -> None, qs
    | Some x -> Some x, {qs with set = IntSet.remove x s}

(* create a queue set from a list. *)
let qs l =
  qs_push {queue = Queue.create (); set = IntSet.empty} l

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
              | Some {leaf} when not (IntSet.mem leaf all_leaves) ->
                stl
              | Some {leaf = best_leaf; distance} ->
                (sbl +. distance, best_leaf) :: stl
              | None -> stl)
          []
          (IntMap.find n adjacency_map)
        in
        let ldistm', updated_leaves, rest = match adj with
          | [] -> IntMap.remove n ldistm', updated_leaves, concat_adj n rest
          | adj ->
            let distance, best_leaf = List.min adj in
            let new_ldist = {leaf = best_leaf; distance} in
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

let fold f initial {tree; ldistm} =
  let bl = Gtree.get_bl tree in
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
  aux initial [tree.Gtree.stree]

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
    (fun {start; finish} -> start >= pos && pos >= finish)
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
            (fun accum {I.mass} -> mass :: accum)
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

(* find the work it takes to move the mass in the mass map to the specified
 * leaf in the voronoi diagram. *)
let leaf_work ?(p_exp = 1.) v indiv_map leaf =
  if not (IntMap.mem leaf indiv_map) then 0.0 else
    let indiv = IntMap.find leaf indiv_map in
    let squashed_indiv = IntMap.singleton
      leaf
      [{I.distal_bl = 0.0; I.mass = I.total_mass indiv}]
    in
    Kr_distance.dist v.tree p_exp indiv squashed_indiv

let adcl ?p_exp v indiv_map =
  IntSet.fold
    (leaf_work ?p_exp v indiv_map |- (+.))
    v.all_leaves
    0.


(* voronoi' *)

(* a partial solution to the full voronoi algorithm *)
type partial_solution = {
  leaf_set: IntSet.t;
  cl_dist: float;
  wk_subtot: float;
  prox_mass: float option;
  interval: (float * float) option;
}

(* a solution to any variant of the voronoi algorithm *)
type solution = {
  leaves: IntSet.t;
  work: float;
}
type solutions = solution IntMap.t

let cl_dist {cl_dist} = cl_dist
let leaf_set {leaf_set} = leaf_set
let leaf_card {leaf_set} = IntSet.cardinal leaf_set
let prox_mass {prox_mass} = Option.default 0. prox_mass
let wk_subtot {wk_subtot} = wk_subtot
let set_interval lhs rhs sol = {sol with interval = Some (lhs, rhs)}
let clear_interval sol = {sol with interval = None}

let sleaves {leaves} = leaves
let swork {work} = work

let is_rmd = function
  | {prox_mass = None} -> true
  | _ -> false

let is_rmp = function
  | {prox_mass = Some _} -> true
  | _ -> false

(* from a set of leaves and a tree, produce a map from all nodes on the tree to
 * a map from each leaf to the distance between that node and leaf. *)
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

(* from a tree, return a map from each node to a list of marks on the edge
 * above that node and a map from each node to the distance to both the closest
 * and farthest away leaves proximal to that node. *)
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
     |> List.of_enum
     |> List.fold_left (snd |- fold_both min max |> flip) (infinity, neg_infinity))
    distm

let slope = function
  | {prox_mass = Some y}
  | {cl_dist = y} -> y

let hull_cull ?(verbose = false) lower_bound upper_bound sols =
  if verbose then
    Printf.eprintf " hull cull: %g %g" lower_bound upper_bound;
  let keys, sola = List.map ((slope &&& wk_subtot) &&& identity) sols
    |> List.sort
        (on fst (Tuple2.compare ~cmp1:approx_compare ~cmp2:approx_compare))
    |> List.fold_left
        (fun accum ((m, b), sol) -> match accum with
         | ((b_prev, _), _) :: _ when b <~> b_prev >= 0 -> accum
         | _ -> ((b, m), sol) :: accum)
        []
    |> List.split
    |> (Array.of_list *** Array.of_list)
  in
  if Array.length sola < 2 then begin
    if verbose then
      Printf.eprintf " skipped\n%!";
    Array.enum sola
  end else begin
    if verbose then
      Printf.eprintf " culling %d\n%!" (Array.length sola);
    match Cdd.extreme_vertices (max lower_bound 0.) upper_bound keys with
      | Some culled ->
        let last_i = Array.length culled |> pred in
        Enum.init
          (Array.length culled)
          (fun i ->
            let sol_idx, x, _ = culled.(i) in
            set_interval
              x
              (if i = last_i then upper_bound
               else Tuple3.second culled.(succ i))
              sola.(sol_idx))
      | None ->
        if verbose then
          Printf.eprintf " cddlib failed.\n%!";
        Array.enum sola
          |> Enum.map clear_interval
  end

(* a polymorphic map for keys of int, bool *)
let empty_solmap = Tuple2.compare ~cmp1:(-) ~cmp2:Bool.compare |> Map.create
let solmap_key = leaf_card &&& is_rmp

(* cull solutions from an enum of solutions down to a list of strictly the
 * best solutions per leaf set cardinality. *)
let cull mass_above (minleaf, maxleaf) ?(verbose = false) sols =
  if verbose then
    Printf.eprintf "culling solutions\n%!";
  let count = ref 0 in
  Enum.fold
    (fun solm sol ->
      incr count;
      Map.modify_def [] (solmap_key sol) (List.cons sol) solm)
    empty_solmap
    sols
  |> Map.enum
  |> Enum.map
      (junction
         (fst |- snd)
         (snd |- hull_cull ~verbose minleaf maxleaf)
         (snd |- hull_cull ~verbose 0. mass_above))
  |> Enum.flatten

(* given a mark map, a tree, and mass on the tree, remove redundant marks from
 * the tree. i.e. remove any mark that doesn't have mass on one side of it. *)
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

let map_min f l = List.map f l |> List.min
let map_reduce f_map f_reduce l = List.map f_map l |> List.reduce f_reduce

(* combine across the solutions below an internal node, given a solution list
 * for each node immediately below this node. knowing the max_leaves can help
 * in not having to consider every solution, as they can be pruned off
 * early. *)
let combine_solutions ?(verbose = false) max_leaves solsl =
  if verbose then begin
    Printf.eprintf "combining across ";
    List.print ~first:"" ~last:"\n" ~sep:", "
      Int.print stderr (List.map List.length solsl);
    flush_all ()
  end;
  solsl
  |> EnumFuns.n_cartesian_product
  |> Enum.map
      (fun sols ->
        let leaf_set = map_reduce leaf_set IntSet.union sols in
        if IntSet.cardinal leaf_set > max_leaves then [] else (* ... *)
        let cl_dist = map_min cl_dist sols
        and wk_subtot = map_reduce wk_subtot (+.) sols
        and tot_prox_mass = map_reduce prox_mass (+.) sols in
        let prox_mass =
          if List.for_all is_rmd sols then None else Some tot_prox_mass
        and addition =
          if List.for_all is_rmp sols then None else
            Some {
              leaf_set; cl_dist; prox_mass = None; interval = None;
              wk_subtot = wk_subtot +. cl_dist *. tot_prox_mass}
        in
        [{leaf_set; wk_subtot; cl_dist; prox_mass; interval = None}]
        |> maybe_cons addition)
  |> Enum.map List.enum
  |> Enum.flatten

let soln_to_info mark {leaf_set; cl_dist; prox_mass; wk_subtot; interval} =
  let fmt = Printf.sprintf "%g" in
  [Option.map_default fmt "-" mark;
   IntSet.cardinal leaf_set |> string_of_int;
   fmt cl_dist;
   Option.map_default fmt "RMD" prox_mass;
   fmt wk_subtot;
   Option.map_default (fst |- fmt) "-" interval;
   Option.map_default (snd |- fmt) "-" interval]

let soln_csv_opt = ref None
let csvrow ?mark i sol = match !soln_csv_opt with
  | None -> ()
  | Some ch ->
    string_of_int i :: soln_to_info mark sol
      |> Csv.output_record ch

let base_rmd leaf =
  {leaf_set = IntSet.singleton leaf; prox_mass = None; wk_subtot = 0.;
   cl_dist = 0.; interval = None}
let base_rmp =
  {leaf_set = IntSet.empty; prox_mass = Some 0.; wk_subtot = 0.;
   cl_dist = infinity; interval = None}

(* solve a tree using the full algorithm. *)
let solve ?(verbose = false) gt mass n_leaves =
  let markm, minmaxlm = mark_map gt
  and mass = I.sort mass
  and total_mass = I.total_mass mass in
  let bubbles = collapse_marks gt mass markm
  and get_bl = Gtree.get_bl gt
  and top_id = Gtree.top_id gt in
  let bubbles_of i =
    IntMap.get i [] bubbles
      |> List.enum
      |> flip Enum.append (get_bl i |> Enum.singleton)
  in
  let rec aux tree = Return.with_label (_aux tree)
  and _aux tree lbl =
    let i, mass_below, solutions = match tree with
      | Leaf i -> i, 0., List.enum [base_rmp; base_rmd i]
      | Node (i, subtrees) ->
        let massl, subsols = List.map aux subtrees |> List.split in
        let mass_below = List.fsum massl in
        let cull_fn = if i = top_id then identity
          else cull (total_mass -. mass_below) (IntMap.find i minmaxlm) ~verbose
        in
        if verbose then Printf.eprintf "node %d:\n" i;
        i,
        mass_below,
        List.map List.of_enum subsols
          |> combine_solutions ~verbose n_leaves
          |> cull_fn

    in
    let solutions = Enum.map (tap (csvrow i)) solutions in
    if i = top_id then Return.return lbl (mass_below, solutions);
    let marks = bubbles_of i
    and masses = IntMap.get i [] mass |> List.enum in
    IntMap.get i [] mass
      |> List.fold_left (fun accum {I.mass} -> mass +. accum) mass_below,
    Enum.fold
      (fun (last_mark, solutions) mark ->
        let masses =
          Enum.take_while (fun {I.distal_bl} -> distal_bl <= mark) masses
          |> List.of_enum
        and bub_len = mark -. last_mark in
        let bub_mass = I.v_mass masses
        and wk_distal = I.work_moving_to masses last_mark
        and wk_prox = I.work_moving_to masses mark in
        mark,
        (* Moving through a bubble. *)
        (List.fold_left
          (fun accum -> function
            (* RMD solutions move all mass toward the leaves. *)
            | {prox_mass = None} as sol ->
              (* ... and maybe also start moving mass away from the leaves. *)
              let addition =
                if bub_mass =~ 0. then None else
                  Some {sol with
                    cl_dist = sol.cl_dist +. bub_len;
                    prox_mass = Some bub_mass;
                    wk_subtot = sol.wk_subtot +. wk_prox}
              in
              maybe_cons addition accum
              |> List.cons {sol with
                cl_dist = sol.cl_dist +. bub_len;
                wk_subtot = sol.wk_subtot +. wk_distal +. bub_mass *. sol.cl_dist}
            (* RMP solutions move all mass away from the leaves. *)
            | {prox_mass = Some prox_mass} as sol ->
              {sol with
                prox_mass = Some (prox_mass +. bub_mass);
                wk_subtot = sol.wk_subtot +. wk_prox +. prox_mass *. bub_len;
                cl_dist = sol.cl_dist +. bub_len}
              :: accum)
          []
          solutions)
        |> tap (List.iter (csvrow ~mark i)))
      (0., List.of_enum solutions)
      marks
    |> snd
    |> List.enum
    |> tap
        (fun _ -> match Enum.get masses with
         | None -> ()
         | Some _ -> failwith (Printf.sprintf "unused mass on node %d" i))

  in
  Gtree.get_stree gt |> aux |> snd

(* brute-force a voronoi solution by trying every combination of leaves,
 * calculating the ADCL of each, and choosing the best. *)
let force gt mass ?(strict = true) ?(verbose = false) n_leaves =
  let leaves_adcl leaves =
    let v = of_gtree_and_leaves gt leaves in
    partition_indiv_on_leaves v mass |> adcl v
  in
  Gtree.leaf_ids gt
    |> EnumFuns.powerset
    |> Enum.filter_map
        (function
          | [] -> None
          | l when strict && List.length l <> n_leaves -> None
          | l when List.length l <= n_leaves -> Some (IntSet.of_list l)
          | _ -> None)
    |> Enum.group IntSet.cardinal
    |> Enum.map
        (Enum.map (identity &&& leaves_adcl)
         |- Enum.arg_min snd
         |- (if verbose then
               tap (fst |- IntSet.cardinal |- Printf.eprintf "solved %d\n%!")
             else identity)
         |- (fun (leaves, work) -> IntSet.cardinal leaves, {leaves; work}))
    |> IntMap.of_enum

module type Alg = sig
  val solve:
    Newick_gtree.t -> Mass_map.Indiv.t -> ?strict:bool -> ?verbose:bool -> int -> solutions
end

let best_wk_subtot sol1 sol2 =
  if sol1.wk_subtot <= sol2.wk_subtot then sol1 else sol2

module Full = struct
  let csv_log = soln_csv_opt
  let solve gt mass ?strict:_ ?(verbose = false) n_leaves =
    begin match !csv_log with
    | None -> ()
    | Some ch ->
      Csv.output_record ch
        ["node"; "mark"; "leaf_card"; "cl_dist"; "prox_mass"; "wk_subtot";
         "oinv_lft"; "oinv_rgt"]
    end;
    solve ~verbose gt mass n_leaves
      |> Enum.filter is_rmd
      |> Enum.fold
          (fun accum sol ->
            IntMap.modify_def sol (leaf_card sol) (best_wk_subtot sol) accum)
          IntMap.empty
      |> IntMap.values
      |> Enum.map
          (fun {leaf_set; wk_subtot} ->
            IntSet.cardinal leaf_set,
            {leaves = leaf_set; work = wk_subtot})
      |> IntMap.of_enum
      |> tap (fun _ -> csv_log := None)

end

module Forced = struct
  let solve = force
end

(* update a map with what the ADCL would be if a particular leaf was removed
 * from the voronoi diagram. *)
let update_score indiv v leaf map =
  let v', _ = uncolor_leaf v leaf in
  adcl v' (partition_indiv_on_leaves v' indiv)
  |> flip (IntMap.add leaf) map

module Greedy = struct
  let solve gt mass ?strict:_ ?(verbose = false) n_leaves =
    let rec aux diagram accum score_map updated_leaves lbl =
      if IntSet.cardinal diagram.all_leaves <= n_leaves then
        Return.return lbl accum;
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
          {work = partition_indiv_on_leaves diagram' mass |> adcl diagram';
           leaves = diagram'.all_leaves}
          accum
      in
      aux
        diagram'
        accum'
        (IntMap.remove leaf score_map')
        (IntSet.remove leaf updated_leaves')
        lbl
    in
    let v = of_gtree gt in
    aux
      v
      (IntMap.singleton
         (IntSet.cardinal v.all_leaves)
         {leaves = v.all_leaves;
          work = partition_indiv_on_leaves v mass |> adcl v})
      IntMap.empty
      v.all_leaves
    |> Return.with_label

end

module PAM = struct
  let solve gt mass ?strict:_ ?verbose:_ n_leaves =
    let gt = Newick_gtree.add_zero_root_bl gt in
    let leaves = Pam_solver.solve gt mass n_leaves in
    let diagram = of_gtree_and_leaves gt leaves in
    let work = partition_indiv_on_leaves diagram mass
      |> adcl diagram
    in
    IntMap.singleton n_leaves {leaves; work}

end
