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

module ColorMap = MapsSet.StringMap
type color = string
type cotree = Newick_gtree.t * color option IntMap.t

type mark =
  {
    edge_num: int;
    distal_bl: float;
    color: color;
  }

type cdist =
  {
    color: color;
    distance: float;
  }

type v =
  {
    cot: cotree;
    upper: (mark * (cdist option)) ColorMap.t; (* ? *)
    lower: mark IntMap.t;
    cdistm: cdist IntMap.t;
  }

val of_cotree: cotree -> t
(** Compute the Voronoi diagram where points of interest are the leaves. *)

val uncolor_leaf: v -> color -> v
(** This function returns the updated Voronoi after removing the given color. *)


