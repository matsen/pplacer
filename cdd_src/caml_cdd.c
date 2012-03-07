#include <string.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <stdio.h>

#define Val_none Val_int(0)

value Val_some(value v)
{
    CAMLparam1(v);
    CAMLlocal1(some);
    some = caml_alloc_small(1, 0);
    Field(some, 0) = v;
    CAMLreturn(some);
}

#define Some_val(v) Field(v,0)

double *extreme_vertices(const double *, const size_t, const size_t, float, float, size_t *);

CAMLprim value caml_extreme_vertices(value vertices, value lower_bound, value upper_bound)
{
  CAMLparam3(vertices, lower_bound, upper_bound);
  double *vert_arr = Data_bigarray_val(vertices);
  int nrows = (Bigarray_val(vertices)->dim[0]);
  int ncols = (Bigarray_val(vertices)->dim[1]);
  size_t res_size;
  double *res = extreme_vertices(vert_arr, nrows, ncols,
      Double_val(lower_bound), Double_val(upper_bound), &res_size);
  if (res == NULL)
    CAMLreturn(Val_none);
  else {
    value res_bigarr = alloc_bigarray_dims(BIGARRAY_FLOAT64 | BIGARRAY_C_LAYOUT, 2, NULL, res_size / 3, 3);
    printf("CAML output_size %lu\n", res_size);
    memcpy(Data_bigarray_val(res_bigarr), res, sizeof *res * res_size);
    free(res);
    CAMLreturn(Val_some(res_bigarr));
  }
}
