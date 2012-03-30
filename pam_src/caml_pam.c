#include <string.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <gsl/gsl_matrix.h>

#include <stdio.h>

size_t* pam(gsl_matrix*, size_t, /*OUT*/ double*);

/* rows for leaves; columns for masses */
CAMLprim value caml_pam(value k_value, value dist_value, value weight_value)
{
  CAMLparam2(k_value, dist_value);
  double *dist = Data_bigarray_val(dist_value);
  gsl_matrix_view m;
  double work;
  size_t *medoids, *medoids_ptr;
  size_t nrow, ncol;
  CAMLlocal2(res_bigarr, res_tuple);
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

  /* Create a [bigarray;float] tuple to return */
  res_tuple = caml_alloc(2, 0);
  Store_field(res_tuple, 0, res_bigarr);
  /* Work returned is total. */
  Store_field(res_tuple, 1, caml_copy_double(work));

  CAMLreturn(res_tuple);
}
