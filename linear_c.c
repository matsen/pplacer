#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


CAMLprim value dot_c(value a_value, value b_value, value size_value)
{
  double *a = Data_bigarray_val(a_value);
  double *b = Data_bigarray_val(b_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i=0;
  for(i=0; i < size; i++) {
    tot += a[i] * b[i];
  }
  return caml_copy_double(tot);
}


CAMLprim value triple_dot_c(value x_value, value y_value, value z_value, value size_value)
{
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i=0;
  for(i=0; i < size; i++) {
    tot += x[i] * y[i] * z[i];
  }
  return caml_copy_double(tot);
}


CAMLprim value quad_dot_c(value x_value, value y_value, value z_value, value w_value, value size_value)
{
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  double *w = Data_bigarray_val(w_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i=0;
  for(i=0; i < size; i++) {
    tot += x[i] * y[i] * z[i] * w[i];
  }
  return caml_copy_double(tot);
}


CAMLprim value pairwise_prod_c(value dest_value, value x_value, value y_value, value size_value)
{
  double *dest = Data_bigarray_val(dest_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i;
  for(i=0; i < size; i++) {
    dest[i] = x[i] * y[i];
  }
  return Val_unit;
}

