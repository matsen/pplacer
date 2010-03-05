/* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <math.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>



CAMLprim value log_like3_c(value statd_value, value x_value, value y_value, value z_value, value util_value)
{
  CAMLparam5(statd_value,x_value, y_value, z_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *statd = Data_bigarray_val(statd_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  double *util = Data_bigarray_val(util_value);
  int n_rates = (Bigarray_val(x_value)->dim[0]);
  int n_sites = (Bigarray_val(x_value)->dim[1]);
  int n_states = (Bigarray_val(x_value)->dim[2]);
  int rate, site, state;
  for(site=0; site < n_sites; site++) { util[site] = 0.0; }
  double *util_v;
  for(rate=0; rate < n_rates; rate++) { 
    // for each rate, start at the top of the util vector 
    util_v = util;
    for(site=0; site < n_sites; site++) {
    // proceed through the util vector and the sites
      for(state=0; state < n_states; state++) {
	*util_v += statd[state] * (*x) * (*y) * (*z);
	x++;
	y++;
	z++;
      }
      util_v++;
    }
  }
  // now total up the likes from the util vector
  double ll_tot=0;
  float f_n_rates = (float) n_rates;
  for(site=0; site < n_sites; site++) {
    ll_tot += log(util[site] / f_n_rates);
  }
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value pairwise_prod_c(value dest_value, value x_value, value y_value)
{
  CAMLparam3(dest_value, x_value, y_value);
  double *dest = Data_bigarray_val(dest_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int size = 
    (Bigarray_val(x_value)->dim[0]) 
    * (Bigarray_val(x_value)->dim[1])
    * (Bigarray_val(x_value)->dim[2]);
  int i;
  for(i=0; i < size; i++) {
    dest[i] = x[i] * y[i];
  }
  CAMLreturn(Val_unit);
}

CAMLprim value glv_print_c(value x_value)
{
  CAMLparam1(x_value);
  double *x = Data_bigarray_val(x_value);
  int n_rates = (Bigarray_val(x_value)->dim[0]);
  int n_sites = (Bigarray_val(x_value)->dim[1]);
  int n_states = (Bigarray_val(x_value)->dim[2]);
  double *loc = x;
  int rate, site, state;
  for(rate=0; rate < n_rates; rate++) { 
    for(site=0; site < n_sites; site++) {
      for(state=0; state < n_states; state++) {
	printf("%g\t",*loc);
	loc++;
      }
      printf("\n");
    }
    printf("--\n");
  }
  CAMLreturn(Val_unit);
}
