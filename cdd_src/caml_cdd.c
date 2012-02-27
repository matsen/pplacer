#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

double *extreme_vertices(const double *, const size_t, const size_t, size_t *);

CAMLprim value caml_extreme_vertices(value vertices)
{
  CAMLparam1(vertices);
  double *vert_arr = Data_bigarray_val(vertices);
  int nrows = (Bigarray_val(vertices)->dim[0]);
  int ncols = (Bigarray_val(vertices)->dim[1]);
  size_t res_size;
  double *res = extreme_vertices(vert_arr, nrows, ncols, &res_size);
  value res_bigarr = alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT, 2, res, res_size / 3, 3);
  free(res);
  CAMLreturn(res_bigarr);
}
