open Ppatteries

type t

val figs_of_gtree: float -> Newick_gtree.t -> t
val enum_by_score: (int -> 'a) -> t -> int Enum.t
val length: t -> int
