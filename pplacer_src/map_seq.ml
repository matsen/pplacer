(* For calculating Maximum A Posteriori sequences for internal locations on the
 * tree. *)


type pos = Distal | Proximal

(* first we need to grab u1 and u2. These can be some snodes.(0) and snodes.(1)
 * (fail nicely if these are out of bounds, which means that the ref tree has <
 * 1 leaf. *)

let of_map u1 u2 model t ~darr ~parr m =
  IntMap.mapi
    (fun id, _ ->
      Mutpick.get_summary
        Mutpick.Proximal
        Gsl_vector.max_index
        (-1) u1 u2 model t
        ~darr ~parr id)
    m

(* apply this one to the mrcam ... *)
