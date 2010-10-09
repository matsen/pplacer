type t = int
val max_bits : int
val empty : t
val of_int : int -> t
val to_int : t -> int
val compare : t -> t -> int
val check_bounds : int -> unit
val get : t -> int -> bool
val ei : int -> t
val set : t -> int -> t
val clear : t -> int -> t
val get_pos_indices: t -> int list
val to_string : t -> string
val ppr : Format.formatter -> t -> unit

