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

open Ppatteries

(* leaves are ints since the node numbers on an stree are ints. *)
type leaf = int

(* a leaf and the distance to that leaf. *)
type ldist = {
  leaf: leaf;
  distance: float;
}

(* a map from nodes on a tree to the associated ldists. *)
type ldistm = ldist IntMap.t

(* a voronoi diagram: a tree, the leaves on the tree being considered, and the
 * ldistm on the tree. The set of leaves in the ldistm is a strict subset of the
 * all_leaves set. *)
type v = {
  tree: Newick_gtree.t;
  ldistm: ldist IntMap.t;
  all_leaves: IntSet.t;
}

(* A portion of an edge defined by (id, start, finish), where id is the edge
 * id, start is where it starts, and finish is where it ends (both are measured
 * from distal side; start >= finish). *)
type snip = {
  assoc_leaf: int;
  distal_edge: int;
  proximal_edge: int;
  start: float;
  finish: float;
}

val adjacent_bls: Newick_gtree.t -> (int * float) list IntMap.t
(** Generate a map from each id in a Newick_gtree.t to a list of (neighbor_id,
    distance) pairs for every neighboring node for the given node, where the
    distance is the edge length between the two nodes. The parent of a node
    counts as a neighbor. *)

val update_ldistm: ldistm -> IntSet.t -> int list -> Newick_gtree.t -> ldistm * IntSet.t
(** Given an ldistm, a set of currently valid leaves, the leaves at which to
    start updating, and a tree on which the leaves are placed, update the
    ldistm to reflect only the valid leaves. *)

val of_gtree: Newick_gtree.t -> v
(** Compute the Voronoi diagram where points of interest are the leaves. *)

val of_gtree_and_leaves: Newick_gtree.t -> IntSet.t -> v
(** Compute the Voronoi diagram where points of interest are only the leaves
    specified. *)

val uncolor_leaves: v -> IntSet.t -> v * IntSet.t
(** This function returns the updated Voronoi after removing all of the leaves
    in the given set, as well as the set of leaves which were affected by this
    removal. *)
val uncolor_leaf: v -> leaf -> v * IntSet.t
(** uncolor_leaf v l <=> uncolor_leaves v (IntSet.singleton l) *)

val fold: ('a -> snip -> 'a) -> 'a -> v -> 'a
(** Effectively fold over the edge_snipl defined below, but we don't have to
    construct it in memory. *)

val get_edge_snipl: v -> leaf -> snip list
(** Get a list of the edge_snips that are of the given leaf in v. *)

val matching_snip: snip list -> float -> snip
(** Find the snip in the list which covers the specified position. *)

val get_snipdist: v -> snip list IntMap.t
(** Find all of the snips on the tree of a voronoi diagram. *)

val partition_indiv_on_leaves: v -> Mass_map.Indiv.t -> Mass_map.Indiv.t IntMap.t
(** Given a voronoi diagram and mass map, generate a map from leaves to mass
    maps containing only the mass on that leaf. *)

val distribute_mass: v -> Mass_map.Indiv.t -> float list IntMap.t
(** Given a voronoi diagram and mass map, distribute the mass onto all of the
    leaves in the diagram. The result maps leaf numbers to a lists of all mass
    placed onto that leaf. *)

val placement_distance: v -> ?snipdist:snip list IntMap.t -> Placement.placement -> float
(** Find the distance from the specified placement to the closest leaf on a
    voronoi diagram. If a snipdist isn't provided, it will be calculated from
    the specified diagram. *)

val ecld: ?p_exp:float -> v -> Mass_map.Indiv.t IntMap.t -> float
(** Find the ECLD of a voronoi diagram, given a mapping from leaves to mass on
    the tree being moved to the leaf. o*)

type solution = {
  leaves: IntSet.t;
  work: float;
}
type solutions = solution IntMap.t
val sleaves: solution -> IntSet.t
val swork: solution -> float

module type Alg = sig
  val solve:
    Newick_gtree.t -> Mass_map.Indiv.t -> ?strict:bool -> ?verbose:bool -> int -> solutions
end

module Greedy: Alg
(** Greedy algorithm: at each step, remove the leaf which reduces the ECLD by
    the most. *)

module Full: sig
  include Alg
  val csv_log: Csv.out_channel option ref
(** If csv_log is Some out_channel, then a log of the solutions considered at
    every internal node is written out in CSV format. *)

end
(** Full algorithm: determine the set of leaves which produces the smallest
    ECLD by examining the tree. *)

module Forced: Alg
(** Forced algorithm: brute-force to try every combination of leaves looking
    for which has the smallest ECLD. *)

module PAM: Alg
(** PAM algorithm. *)
