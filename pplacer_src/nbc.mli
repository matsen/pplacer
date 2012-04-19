exception Invalid_base of char

val bases: string
val informative: char -> bool
val word_to_int: string -> int
val int_to_word: ?word_length:int -> int -> string
val gen_count_by_seq: int -> (int -> unit) -> string -> unit

val max_word_length: int

module Preclassifier: sig
  type 'a t
  exception Tax_id_not_found of Tax_id.t
  val make: (int, 'a) Bigarray.kind -> int -> Tax_id.t array -> 'a t
  val tax_id_idx: 'a t -> Tax_id.t -> int
  val add_seq: 'a t -> Tax_id.t -> string -> unit
end

module Classifier: sig
  type t
  val make: ?n_boot:int -> ?map_file:(Unix.file_descr * bool) -> 'a Preclassifier.t -> t
  val classify: t -> string -> Tax_id.t
  val bootstrap: t -> string -> float Tax_id.TaxIdMap.t
  val of_refpkg:
    ?ref_aln:Alignment.t -> ?n_boot:int -> ?map_file:(Unix.file_descr * bool) -> int -> int -> Refpkg.t -> t
end
