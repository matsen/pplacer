(** Voronoi diagrams on phylogenetic tree.
 *
 * "Marks" are where labels are changed in the diagram, and labels flow up the
 * tree. That is, a mark sets the label of all edges proximal to that mark until
 * another mark is encountered.
 *
 * The implementation will be set up for situations where we only have
 * at most one boundary per edge, as when the points of interest are nodes of
 * the phylogenetic tree.
 *
 *)

type label = string
type mark = float * label  (* The float is the distal branch length. *)
type mark_map = mark IntMap.t

val leaf_vd_of_gtree: Newick_gtree.t -> mark_map
(** Compute the Voronoi diagram where points of interest are the leaves. *)
