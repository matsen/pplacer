open Ppatteries

let merge = const incr |> flip
let lmerge = List.fold_left ((!) |- (+) |> flip) 0 |- ref

module I = Mass_map.Indiv

(* Convenience wrapper around the functions in Kr_distance for specifying just
 * a callback with parameters for the count of edge segments below the current
 * edge segment and the branch length. The return values of the callbacks are
 * summed across the tree. *)
let count_along_mass gt mass cb =
  let partial_total id =
    Kr_distance.total_along_edge
      cb
      (Gtree.get_bl gt id)
      (IntMap.get id [] mass |> List.map I.to_pair |> List.sort compare)
      merge
  in
  Kr_distance.total_over_tree
    partial_total
    (const ())
    lmerge
    (fun () -> ref 0)
    (Gtree.get_stree gt)

let k_maps_of_placerun: int -> Newick_bark.t Placerun.t -> float IntMap.t IntMap.t
= fun k_max pr ->
  let n = Placerun.get_pqueries pr |> List.length in
  let n' = float_of_int n
  and base_k_map = 0 -- n
    |> Enum.map (identity &&& const 1.)
    |> IntMap.of_enum
    |> IntMap.singleton 0
  in
  Enum.fold
    (fun k_maps k ->
      let prev_map = IntMap.find k k_maps
      and diff = n' -. float_of_int k in
      let q_k r = (diff -. float_of_int r) /. diff *. IntMap.find r prev_map in
      0 -- n
        |> Enum.map (identity &&& q_k)
        |> IntMap.of_enum
        |> flip (IntMap.add (k + 1)) k_maps)
    base_k_map
    (0 --^ k_max)

(* Compute the rarefaction curve of a placerun, given a placement criterion and
 * optionally the highest X value for the curve. *)
let of_placerun:
    (Placement.t -> float) -> ?k_max:int -> Newick_bark.t Placerun.t -> (int * float) Enum.t
= fun criterion ?k_max pr ->
  let gt = Placerun.get_ref_tree pr |> Newick_gtree.add_zero_root_bl
  and mass = I.of_placerun
    Mass_map.Point
    criterion
    pr
  in
  let n = Placerun.get_pqueries pr |> List.length in
  let k_max = match k_max with
    | Some k when k < n -> k
    | _ -> n
  in
  let k_maps = k_maps_of_placerun k_max pr in
  let count k =
    let q_k = IntMap.find k k_maps |> flip IntMap.find in
    count_along_mass
      gt
      mass
      (fun d bl ->
        let d = !d in
        let p = n - d in
        (1. -. (q_k d) -. (q_k p)) *. bl)
  in
  2 -- k_max
    |> Enum.map (identity &&& count)

let mass_induced_tree gt mass =
  let edge = ref 0
  and bark_map = ref IntMap.empty
  and mass_counts = ref IntMap.empty in
  let next_edge mass_count bl =
    let x = !edge in
    incr edge;
    bark_map := Newick_bark.map_set_bl x bl !bark_map;
    if mass_count > 0 then
      mass_counts := IntMap.add x mass_count !mass_counts;
    x
  in
  let open Stree in
  let rec aux t =
    let i, below = match t with
      | Leaf i -> i, None
      | Node (i, subtrees) -> i, Some (List.map aux subtrees)
    in
    let ml = IntMap.get i [] mass
      |> List.map (fun {I.distal_bl} -> distal_bl)
      |> List.cons 0.
      |> List.group approx_compare
      |> List.map
          (function
           | hd :: tl when hd =~ 0. -> hd, List.length tl
           | hd :: tl -> hd, List.length tl + 1
           | [] -> invalid_arg "ml")
    and bl = Gtree.get_bl gt i in
    let rec snips tree pl =
      let j, tl = match pl with
        | (p1, c1) :: ((p2, _) :: _ as tl) -> next_edge c1 (p2 -. p1), tl
        | [p, c] -> next_edge c (bl -. p), []
        | [] -> invalid_arg "snips"
      in
      let tree' = Some
        [match tree with
         | None -> leaf j
         | Some subtrees -> node j subtrees]
      in
      match tl with
      | [] -> tree'
      | tl -> snips tree' tl
    in
    snips below ml
    |> Option.get
    |> List.hd
  in
  let st' = aux (Gtree.get_stree gt) in
  !mass_counts, Gtree.gtree st' !bark_map

let distal_proximal_maps st marks_map =
  let open Stree in
  let rec aux = function
    | Leaf i -> IntMap.singleton i (IntMap.get i 0 marks_map)
    | Node (i, subtrees) ->
      let distal_marks = List.map aux subtrees |> List.reduce IntMap.union in
      let marks = List.enum subtrees
        |> Enum.map (top_id |- flip IntMap.find distal_marks)
        |> Enum.sum
        |> (+) (IntMap.get i 0 marks_map)
      in
      IntMap.add i marks distal_marks
  in
  let distal_map = aux st in
  let total_marks = IntMap.values marks_map |> Enum.sum in
  let proximal_map = IntMap.map ((-) total_marks) distal_map in
  distal_map, proximal_map

let distal_edges_map st =
  let open Stree in
  let rec aux = function
    | Leaf i -> IntMap.singleton i IntSet.empty
    | Node (i, subtrees) ->
      let distal_edges = List.map aux subtrees |> List.reduce IntMap.union in
      let subtree_edges = List.map top_id subtrees in
      let edges = List.enum subtree_edges
        |> Enum.map (flip IntMap.find distal_edges)
        |> Enum.reduce IntSet.union
        |> IntSet.union (IntSet.of_list subtree_edges)
      in
      IntMap.add i edges distal_edges
  in
  aux st

exception What of (int * int * int * int)

let auto_cache ?(count = 1024 * 1024) f =
  curry (Cache.make_ht ~gen:(uncurry f) count).Cache.get

let variance_of_placerun criterion pr =
  let gt = Placerun.get_ref_tree pr |> Newick_gtree.add_zero_root_bl
  and mass = I.of_placerun
    Mass_map.Point
    criterion
    pr
  in
  let n = Placerun.get_pqueries pr |> List.length in
  let k_max = n in
  let k_maps = k_maps_of_placerun k_max pr in
  let marks_map, gt' = mass_induced_tree gt mass in
  let bl = Gtree.get_bl gt' in
  let st' = Gtree.get_stree gt' in
  let distal_marks, proximal_marks = distal_proximal_maps st' marks_map in
  let distal_edges = distal_edges_map st' in
  (* is i proximal to j? *)
  let _is_proximal i j = IntSet.mem j (IntMap.find i distal_edges) in
  let is_proximal = auto_cache _is_proximal in
  let _o i j =
    if is_proximal i j then IntMap.find i proximal_marks
    else if is_proximal j i then IntMap.find i distal_marks
    else IntMap.find i distal_marks
  and _s i j =
    if is_proximal i j then IntMap.find i distal_marks
    else if is_proximal j i then IntMap.find i proximal_marks
    else IntMap.find i proximal_marks
  and _q k x = IntMap.find k k_maps |> IntMap.find x in
  let o = auto_cache _o and s = auto_cache _s and q = auto_cache _q in
  let cov k i j =
    let q_k = q k in
    let union = if i = j then o i j else o i j + o j i in
    q_k union -. q_k (o i j) *. q_k (o j i)
      +. (1. -. q_k (o i j)) *. q_k (s j i)
      +. (1. -. q_k (o j i)) *. q_k (o j i)
      -. q_k (s i j) *. q_k (s j i)
  in
  let cov_times_bl k i j =
    cov k i j *. bl i *. bl j
  in
  let n_edges = Stree.top_id st' in
  let var k =
    Uptri.init n_edges (cov k)
      |> Uptri.ppr_lowtri Format.std_formatter Format.pp_print_float;

    let diag = 0 --^ n_edges
      |> Enum.map (fun i -> cov_times_bl k i i)
      |> Enum.fold (+.) 0.
    in
    Uptri.init n_edges (cov_times_bl k)
      |> Uptri.fold_left (+.) 0.
      |> ( *.) 2.
      |> (+.) diag
  in
  2 -- k_max
    |> Enum.map (identity &&& var)
