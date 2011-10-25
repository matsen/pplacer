open Ppatteries

type t

val figs_of_gtree: float -> Newick_gtree.t -> t
val enum_by_score: (int -> 'a) -> t -> ('a * int) Enum.t
val enum_all: t -> int Enum.t
val length: t -> int
val onto_decor_gtree: Decor_gtree.t -> t -> Decor_gtree.t
