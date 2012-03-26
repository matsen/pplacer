#include <string.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <gsl/gsl_matrix.h>

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

size_t* pam(gsl_matrix*, size_t, /*OUT*/ float*);

CAMLprim value caml_extreme_vertices(value dist_value, value k)
{
  CAMLparam2(distances, k);
  double *dist = Data_bigarray_val(dist_value);
  gsl_matrix_view *m;
  float work;
  size_t *medoids;
  size_t nrow, ncol;
  nrow = Bigarray_val(dist_value)->dim[0]);
  ncol = Bigarray_val(dist_value)->dim[1]);
  m = gsl_matrix_view_array(dist, nrow, ncol);

  medoids = pam(&(m->matrix), Int_value(k), &work);

  CAMLreturn(Val_none);
}
