
(** Code for voronoi diagrams on phylogenetic tree.
 *)

type label = string
type mark = float * label (* distal bl *)
type mark_map = mark IntMap.t

(* we are definitely saying here that we can only have one mark per edge... *)

