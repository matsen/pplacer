#include <string.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <gsl/gsl_matrix.h>

#include <stdio.h>

size_t* pam(gsl_matrix*, size_t, /*OUT*/ float*);

/* rows for leaves; columns for masses */
CAMLprim value caml_pam(value k_value, value dist_value)
{
  CAMLparam2(k_value, dist_value);
  double *dist = Data_bigarray_val(dist_value);
  gsl_matrix_view m;
  float work;
  size_t *medoids, *medoids_ptr;
  size_t nrow, ncol;
  value res_bigarr;
  intnat *res_ptr;
  int i, k;
  nrow = Bigarray_val(dist_value)->dim[0];
  ncol = Bigarray_val(dist_value)->dim[1];
  m = gsl_matrix_view_array(dist, nrow, ncol);
  k = Int_val(k_value);
  medoids = pam(&m.matrix, k, &work);

  res_bigarr = alloc_bigarray_dims(BIGARRAY_CAML_INT | BIGARRAY_C_LAYOUT, 1, NULL, k);
  res_ptr = Data_bigarray_val(res_bigarr);
  medoids_ptr = medoids;
  for (i = 0; i < k; ++i) {
    *res_ptr++ = *medoids_ptr++;
  }
  free(medoids);
  CAMLreturn(res_bigarr);
}
