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

