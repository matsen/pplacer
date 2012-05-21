(*
 * Code implementing independent contrasts, a la J Felsenstein, American
 * Naturalist, 1985.
 *
 * The goal is to pre-calculate enough information on a branch such that one can
 * easily infer a value for a placement. To do this, we will build up maps as
 * follows.
 *
 * Because we will effectively need to "re-root" easily, we need to separate out
 * the "extra" branch length computed by his procedure and the original branch
 * length. We will call this extra branch length "virtual branch length." We
 * will be talking about IC pairs (x,vbl), where x is the value of interest, and
 * vbl is the virtual branch length. These IC pairs will be denoted pi.
 * According to (3) of Felsenstein, and the sentence after, the combination is
 * done:
 *)

let actual_combo (xi, vbli) (xj, vblj) =
  ((xi /. vbli +. xj /. vblj) /. (1. /. vbli +. 1. /. vblj),
    (vbli *. vblj) /. (vbli +. vblj))

(* Note that we don't add in the distal branch length ($v_k$ in his notation).
 * Adding these beasts is like usual: *)

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

(* l is a list of pairs (p1, b1), ..., (pk, bk) *)
let join_subsoln_list l =
  (* this is just a fold *)

(* So, to get all of the distal p's, just do a DFS recursion across the tree.
 * The base case for something that is not labeled with a character (e.g. a copy
 * number) is None, whereas if there is something x given then the base case is
 * Some (x,0). At every internal node, do a join_subsoln_list.
 *)
let build_distal_map t leaf_values =
  (* build a map of all internal nodes to the corresponding p's *)

(* Now build a proximal_map using this proximal map.
 * This will be a recursion as follows: say we are at an internal node such that
 * pd is the distal pair coming from the previous step of the recursion, and bd
 * is the branch length connecting this internal node to the proximal part of
 * the tree.
 * Now, say (p1, b1), ..., (pk, bk) are below us. proximal p for edge i will be
 * join_subsoln_list [p1, b1; ...; p(i-1), b(i-1); p(i+1), b(i+1); ...; pk, bk; pd, bd]
 *)
let build_proximal_map t distal_map =
