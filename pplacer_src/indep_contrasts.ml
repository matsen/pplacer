(*
 * Code implementing independent contrasts, a la J Felsenstein, American
 * Naturalist, 1985.
 *
 * The goal is to pre-calculate enough information on a branch such that one can
 * easily infer a value for a placement. To do this, we will build up maps for
 * the distal and proxmimal sides of each edge.
 *
 * Felsenstein describes a trick on p.10 to add "extra" branch length to the
 * tree in order to describe an increase of uncertainty brought on by estimating
 * values rather than having them on hand. Because we will effectively need to
 * "re-root" easily, we need to separate out the "extra" branch length computed
 * by his procedure and the original branch length. We will call this extra
 * branch length "virtual branch length." We will be talking about IC pairs
 * (x, vbl), where x is the value of interest, and vbl is the virtual branch
 * length. These IC pairs will be denoted pi, pj, etc. According to (3) of
 * Felsenstein, and the sentence after, the combination is done as follows
 * (note that we don't add in the distal branch length $v_k$ in his notation):
 *)

open Ppatteries
module IME = IntMap.Exceptionless

let actual_combo (xi, vbli) (xj, vblj) =
  (xi /. vbli +. xj /. vblj) /. (1. /. vbli +. 1. /. vblj),
  (vbli *. vblj) /. (vbli +. vblj)

(* Adding these beasts is like usual: *)
let actual_sum (xi, vbli) (xj, vblj) = (xi +. xj, vbli +. vblj)

(* There is also the case of missing values. If a value is missing we can simply
 * ignore it. *)
let binary_opt_extend f oa ob = match (oa, ob) with
  | (Some a, Some b) -> Some (f a b)
  | (Some _, None) -> oa
  | (None, Some _) -> ob
  | (None, None) -> None

let combo = binary_opt_extend actual_combo
let sum = binary_opt_extend actual_sum

let bl_sum p bl =
  sum p (Some (0., bl))

(* px is a pair, as above, and bx is a branch length.*)
let join_two_subsolns pi bi pj bj =
  let no_bl b = function
    | Some (_, vb) -> b +. vb =~ 0.
    | None -> false
  in
  if no_bl bi pi then pi
  else if no_bl bj pj then pj
  else combo (bl_sum pi bi) (bl_sum pj bj)

(* l is a list of pairs (p1, b1), ..., (pk, bk) that come up from a
 * multifurcation. Note that we simply consider a multifurcation to be an
 * internal node of length zero.
 *
 *   0  /\
 *     /  \
 *    /\   \
 *   /  \   \
 *  p1  p2  p3
 *
 * We don't have to extend the join of p1 and p2 with any branch length before
 * combining with p3, so this ends up just being
 * combo (combo (sum p1 (0., b1)) (sum p2 (0., b2))) (sum p3 (0., b3))
 * noting that we didn't have to add anything to the internal combo before the
 * next step because of the zero branch length. In general, it's just a fold.
 *   *)
let join_subsoln_list l =
  List.enum l
    |> Enum.map (uncurry bl_sum)
    |> Enum.reduce combo

(* So, to get all of the distal p's, just do a DF recursion across the tree.
 * The base case for something that is not labeled with a character (e.g. a copy
 * number) is None, whereas if there is something x given then the base case is
 * Some (x,0). At every internal node, do a join_subsoln_list.
 *)

let build_distal_map gt leaf_values =
  let bl = Gtree.get_bl gt in
  let open Stree in
  let rec aux = function
    | Leaf i ->
      begin match IME.find i leaf_values with
      | Some x -> (Some (x, 0.), bl i), IntMap.singleton i (x, 0.)
      | None -> (None, bl i), IntMap.empty
      end
    | Node (i, subtrees) ->
      let sol, map = List.map aux subtrees
        |> List.split
        |> join_subsoln_list *** List.reduce IntMap.union
      in
      (sol, bl i), IntMap.add i (Option.get sol) map
  in
  Gtree.get_stree gt |> aux |> snd

(* Now build a proximal_map using this distal map.
 * This will be a recursion as follows: say we are at an internal node such that
 * pp is the proximal pair coming from the previous step of the recursion, and bp
 * is the branch length connecting this internal node to the proximal part of
 * the tree. Now, say (p1, b1), ..., (pk, bk) are the distal pairs below us.
 *
 * ASCII art, with v and ^ representing subtrees:
 *
 *    pp
 *     v
 *     |
 *    /|\
 *   / | \
 *  ^  ^  ^
 * p1 pi  pk
 *
 proximal p for edge i will skip i and add d at the end:
 * join_subsoln_list [p1, b1; ...; p(i-1), b(i-1); p(i+1), b(i+1); ...; pk, bk; pp, bp]
 *)
let build_proximal_map gt distal_map =
  let bl = Gtree.get_bl gt in
  let open Stree in
  let top = Gtree.top_id gt in
  let rec aux accum = function
    | [] -> accum
    | Leaf _ :: rest -> aux accum rest
    | Node (i, subtrees) :: rest ->
      let prox_edge = if i = top then None else Some (IME.find i accum, bl i) in
      let accum' = List.fold_left
        (fun accum t ->
          let j = top_id t in
          let sol = List.remove subtrees t
            |> List.map (top_id |- (flip IME.find distal_map &&& bl))
            |> maybe_cons prox_edge
            |> join_subsoln_list
          in
          IntMap.add j (Option.get sol) accum)
        accum
        subtrees
      in
      aux accum' (List.append rest subtrees)
  in
  aux IntMap.empty [Gtree.get_stree gt]

let of_criterion_map criterion leaf_copy_map pr =
  let gt = Placerun.get_ref_tree pr in
  let dist_cm = Newick_gtree.label_to_leaf_map gt leaf_copy_map
    |> build_distal_map gt
  in
  let prox_cm = build_proximal_map gt dist_cm in
  let copy_of_placement p =
    let loc = Placement.location p
    and length = Placement.distal_bl p in
    let bl = Gtree.get_bl gt loc in
    join_two_subsolns
      (IME.find loc dist_cm)
      length
      (IME.find loc prox_cm)
      (bl -. length)
    |> Option.get
    |> fst
  in
  List.map
    (fun pq ->
      pq,
      List.map
        ((criterion &&& copy_of_placement) |- uncurry ( *.))
        (Pquery.place_list pq)
      |> List.fsum)
    (Placerun.get_pqueries pr)

let scale_placerun criterion leaf_copy_map pr =
  of_criterion_map criterion leaf_copy_map pr
    |> List.map (fun (pq, x) ->
      Pquery.namlom pq
        |> List.map (fun (n, m) -> n, m /. x)
        |> Pquery.set_namlom pq)
    |> Placerun.set_pqueries pr
