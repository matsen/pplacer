(* A map equipped with algebraic operations.
 *
 * not super-high performance because of map and boxing.
 *
 *)

module AlgMap (N: Number.NUMBER) (OT: Map.OrderedType) =
  struct
    module M = Map.Make(OT)
    type t = N.t M.t

    let soft_find k m =
      try M.find k m with
      | Not_found -> N.zero

      (* sets k to (soft value of k) op v *)
    let soft_op_add op k v m =
      M.add k (op (soft_find k m) v) m

    let add_by = soft_op_add (N.add)
    let sub_by = soft_op_add (N.sub)
    let mul_by = soft_op_add (N.mul)
    let div_by = soft_op_add (N.div)
    let max_by = soft_op_add (N.max)
    let pow_by = soft_op_add (N.pow)
  end

module AlgMapB = AlgMap (Number.B)
module AlgMapZ = AlgMap (Number.Z)
module AlgMapR = AlgMap (Number.R)


module IntAlgMapR = AlgMapR (MapsSets.OrderedInt)
