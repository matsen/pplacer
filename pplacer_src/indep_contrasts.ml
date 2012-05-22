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

let actual_combo (xi, vbli) (xj, vblj) =
  ((xi /. vbli +. xj /. vblj) /. (1. /. vbli +. 1. /. vblj),
    (vbli *. vblj) /. (vbli +. vblj))

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

(* px is a pair, as above, and bx is a branch length.*)
let join_two_subsolns pi bi pj bj =
  combo (sum pi (0., bi)) (sum pj (0., bj))

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
  (* this will just be a fold *)

(* So, to get all of the distal p's, just do a DF recursion across the tree.
 * The base case for something that is not labeled with a character (e.g. a copy
 * number) is None, whereas if there is something x given then the base case is
 * Some (x,0). At every internal node, do a join_subsoln_list.
 *)
let build_distal_map t leaf_values =
  (* build a map of all internal nodes to the corresponding p's by a recursion
   * across the tree*)

(* Now build a proximal_map using this distal map.
 * This will be a recursion as follows: say we are at an internal node such that
 * pd is the distal pair coming from the previous step of the recursion, and bd
 * is the branch length connecting this internal node to the proximal part of
 * the tree. Now, say (p1, b1), ..., (pk, bk) are below us.
 *
 * ASCII art, with v and ^ representing subtrees:
 *
 *    pd
 *     v
 *     |
 *    /|\
 *   / | \
 *  ^  ^  ^
 * p1 pi  pk
 *
 proximal p for edge i will skip i and add d at the end:
 * join_subsoln_list [p1, b1; ...; p(i-1), b(i-1); p(i+1), b(i+1); ...; pk, bk; pd, bd]
 *)
let build_proximal_map t distal_map =
  (* A recursion assigning things as we head down the tree (versus up the tree
   * like build_distal_map) *)
