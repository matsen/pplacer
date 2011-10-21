open Ppatteries

type t = int * IntSet.t

val figs_of_gtree: float -> Newick_gtree.t -> t list
val enum_by_score: t list -> (int -> 'a) -> int Enum.t
