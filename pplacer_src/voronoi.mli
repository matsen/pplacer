(** Voronoi diagrams for phylogenetic trees with input set leaves.
 *
 * "Marks" are where colors are changed in the diagram, and colors flow up the
 * tree. That is, a mark sets the color of all edges proximal to that mark until
 * another mark is encountered.
 *
 * We only care about VDs when the set of interesting points are leaves or some
 * subset thereof. Thus we have at most one boundary per edge, as when the
 * points of interest are nodes of the phylogenetic tree.
 *
 *)

open MapsSets

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

(* A portion of an edge defined by (id, start, finish), where id is the edge
 * id, start is where it starts, and finish is where it ends (both are measured
 * from proximal side; start <= finish). *)
type edge_snip = int * float * float

val list_min: ?key:('a -> 'a -> int) -> 'a list -> 'a
val adjacent_bls: Newick_gtree.t -> (int * float) list IntMap.t
val update_ldistm: ldistm -> IntSet.t -> int list -> Newick_gtree.t -> ldistm * IntSet.t

val of_gtree: Newick_gtree.t -> v
(** Compute the Voronoi diagram where points of interest are the leaves. *)

val uncolor_leaf: v -> leaf -> v * IntSet.t
(** This function returns the updated Voronoi after removing the given leaf, as
 * well as returning the leaves that were affected by this removal. *)

val fold: v -> leaf -> ('a -> edge_snip -> 'a) -> 'a -> 'a
(** Effectively fold over the edge_snipl defined below, but we don't have to
 * construct it in memory.
 *)

val get_edge_snipl: v -> leaf -> edge_snip list
(** Get a list of the edge_snips that are of the given leaf in v. *)
