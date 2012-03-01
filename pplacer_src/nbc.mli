exception Invalid_base of char

val bases: string
val informative: char -> bool
val filter_informative: string -> string
val word_to_int: string -> int
val int_to_word: ?word_length:int -> int -> string

val max_word_length: int

module Preclassifier: sig
  type ('a, 'b) t
  exception Tax_id_not_found of Tax_id.t
  val make: ('a, 'b) Bigarray.kind -> int -> Tax_id.t array -> ('a, 'b) t
  val tax_id_idx: ('a, 'b) t -> Tax_id.t -> int
  val add_seq: ('a, 'b) t -> ('a -> 'a) -> Tax_id.t -> string -> unit
end

module Classifier: sig
  type t
  val make: ('a, 'b) Preclassifier.t -> ('a -> 'a -> 'a) -> ('a -> float) -> t
  val classify: t -> string -> Tax_id.t
end
