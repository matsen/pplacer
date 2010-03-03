/* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>


CAMLprim value dot_c(value x_value, value y_value, value size_value)
{
  CAMLparam3(x_value, y_value, size_value);
  CAMLlocal1(ml_tot);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i;
  for(i=0; i < size; i++) {
    tot += x[i] * y[i];
  }
  ml_tot = caml_copy_double(tot);
  CAMLreturn(ml_tot);
}


CAMLprim value triple_dot_c(value x_value, value y_value, value z_value, value size_value)
{
  CAMLparam4(x_value, y_value, z_value, size_value);
  CAMLlocal1(ml_tot);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i;
  for(i=0; i < size; i++) {
    tot += x[i] * y[i] * z[i];
  }
  ml_tot = caml_copy_double(tot);
  CAMLreturn(ml_tot);
}


CAMLprim value quad_dot_c(value x_value, value y_value, value z_value, value w_value, value size_value)
{
  CAMLparam5(x_value, y_value, z_value, w_value, size_value);
  CAMLlocal1(ml_tot);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  double *w = Data_bigarray_val(w_value);
  int size = Int_val(size_value);
  double tot = 0;
  int i;
  for(i=0; i < size; i++) {
    tot += x[i] * y[i] * z[i] * w[i];
  }
  ml_tot = caml_copy_double(tot);
  CAMLreturn(ml_tot);
}

CAMLprim value pairwise_prod_c(value dest_value, value x_value, value y_value, value size_value)
{
  CAMLparam4(dest_value, x_value, y_value, size_value);
  double *dest = Data_bigarray_val(dest_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int size = Int_val(size_value);
  int i;
  for(i=0; i < size; i++) {
    dest[i] = x[i] * y[i];
  }
  CAMLreturn(Val_unit);
}

CAMLprim value triplewise_prod_c(value dest_value, value x_value, value y_value, value z_value, value size_value)
{
  CAMLparam5(dest_value, x_value, y_value, z_value, size_value);
  double *dest = Data_bigarray_val(dest_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  int size = Int_val(size_value);
  int i;
  for(i=0; i < size; i++) {
    dest[i] = x[i] * y[i] * z[i];
  }
  CAMLreturn(Val_unit);
}


CAMLprim value log_like3_c(value statd_value, value x_value, value y_value, value z_value)
{
  CAMLparam4(statd_value,x_value, y_value, z_value);
  CAMLlocal1(ml_ll_tot);
  double *statd = Data_bigarray_val(statd_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  int n_states = Bigarray_val(statd_value)->dim[0];
  int n_sites = (Bigarray_val(x_value)->dim[0]);
  int n_columns = (Bigarray_val(x_value)->dim[1]);
  int n_rates = n_columns/n_states;
  float f_n_rates = (float) n_rates;
  double site_like, ll_tot;
  printf("%d states and %d rates and %d sites \n",n_states,n_rates,n_sites);
  int state, rate, site;
  double *x_v = x;
  double *y_v = y;
  double *z_v = z;
  for(site=0; site < n_sites; site++) {
    site_like = 0;
    for(rate=0; rate < n_rates; rate++) { 
      for(state=0; state < n_states; state++) {
	site_like += x_v[state] * y_v[state] * z_v[state];
      }
/* the following will take us to the next site as well 
 * when we "fall off" of the current site */
      x_v += n_rates;
      y_v += n_rates;
      z_v += n_rates;
    }
    ll_tot += log(site_like / f_n_rates);
  }
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}
