module BA = Bigarray
module BA2 = BA.Array2

type float_ba2 = (float, BA.float64_elt, BA.c_layout) BA2.t
external c_extreme_vertices: float_ba2 -> float -> float -> float_ba2 = "caml_extreme_vertices"

let extreme_vertices lower_bound upper_bound parr =
  let ba = BA2.create BA.float64 BA.c_layout (Array.length parr) 3 in
  BA2.fill ba (-1.);
  Array.iteri
    (fun i (a, b) -> BA2.unsafe_set ba i 0 a; BA2.unsafe_set ba i 1 b)
    parr;
  let res = c_extreme_vertices ba lower_bound upper_bound in
  Array.init
    (BA2.dim1 res)
    (fun i ->
      let col = BA2.unsafe_get res i in
      int_of_float (col 2), col 0, col 1)
