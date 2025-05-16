type double_mat_bigarr =
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t
type matrix = double_mat_bigarr

val dims: matrix -> int * int
val of_arrays: float array array -> matrix
val to_arrays: matrix -> float array array
val set_all: matrix -> float -> unit
val set_zero: matrix -> unit
val set_id: matrix -> unit
val memcpy: src:matrix -> dst:matrix -> unit
val copy: matrix -> matrix
val row: matrix -> int -> Gsl.Vector.vector
val add: matrix -> matrix -> unit
val sub: matrix -> matrix -> unit
val mul_elements: matrix -> matrix -> unit
val div_elements: matrix -> matrix -> unit
val scale: matrix -> float -> unit
val add_constant: matrix -> float -> unit
val add_diagonal: matrix -> float -> unit
val is_null: matrix -> bool
val swap_rows: matrix -> int -> int -> unit
val swap_columns: matrix -> int -> int -> unit
val swap_rowcol: matrix -> int -> int -> unit
val rect_transpose: matrix -> matrix
val transpose: matrix -> matrix -> unit
val transpose_in_place: matrix -> unit
val create: int -> int -> matrix
val mimic: matrix -> matrix
val dim1: matrix -> int
val dim2: matrix -> int
val get: matrix -> int -> int -> float
val set: matrix -> int -> int -> float -> unit
val blit: matrix -> matrix -> unit
val fill: matrix -> float -> unit
val enum: matrix -> float BatEnum.t
val modify: (float -> float) -> matrix -> unit
val modifyij: (int -> int -> float -> float) -> matrix -> unit
val unsafe_get: matrix -> int -> int -> float
val unsafe_set: matrix -> int -> int -> float -> unit
val fp_classify: matrix -> fpclass
val ppr: Format.formatter -> matrix -> unit
val slice_left: matrix -> int ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
