#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

double *extreme_vertices(const double *, const size_t, const size_t, size_t *);

CAMLprim value caml_extreme_verticies(value verticies)
{
  CAMLparam1(verticies);
  double *vert_arr = Data_bigarray_val(verticies);
  int nrows = (Bigarray_val(x_value)->dim[0]);
  int ncols = (Bigarray_val(x_value)->dim[1]);
  size_t *res_size;
  double *res = extreme_verticies(vert_arr, nrows, ncols, &res_size);
  value res_bigarr = alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT, 1, res, res_size);
  free(res);
  CAMLreturn(res_bigarr);
}
