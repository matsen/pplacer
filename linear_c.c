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

/*
#ifdef __SSE__
#ifndef NO_SSE
#define USE_SSE3
#endif
#endif

#ifdef USE_SSE3
#define ALIGNED __attribute__((aligned(16)))
#define IS_ALIGNED(X) ((((unsigned long) new) & 15L) == 0L)
#include <xmmintrin.h>
#else
#define ALIGNED 
#define IS_ALIGNED(X) 1
#endif


// SSE code more or less copied from FastTree
// not used yet
#ifdef USE_SSE3
inline float mm_sum(register __m128 sum) {
  float f[4] ALIGNED;
  _mm_store_ps(f,sum);
  return(f[0]+f[1]+f[2]+f[3]);
}

// sum(f1*f2*f3*f4) 
float vector_multiply4_sum(float *f1, float *f2, float* f3, float* f4, int n) {
  __m128 sum = _mm_setzero_ps();
  int i;
  for (i = 0; i < n; i += 4) {
    __m128 a1, a2, a3, a4;
    a1 = _mm_load_ps(f1+i);
    a2 = _mm_load_ps(f2+i);
    a3 = _mm_load_ps(f3+i);
    a4 = _mm_load_ps(f4+i);
    sum = _mm_add_ps(_mm_mul_ps(_mm_mul_ps(_mm_mul_ps(a1,a2),a3),a4),sum);
  }
  return(mm_sum(sum));
}
#endif

#ifdef USE_SSE3
	*util_v += vector_multiply4_sum(statd, x, y, z, 4);
#else
#endif

*/


CAMLprim value log_like3_c(value statd_value, value x_value, value y_value, value z_value, value util_value)
{
  CAMLparam5(statd_value, x_value, y_value, z_value, util_value);
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
  double *util_v;
  for(site=0; site < n_sites; site++) { util[site] = 0.0; }
  for(rate=0; rate < n_rates; rate++) { 
    // for each rate, start at the top of the util vector 
    util_v = util;
    // here we hard code in the limits for some popular choices 
    // so that loops can get unrolled
    if(n_states == 4) {
      for(site=0; site < n_sites; site++) {
	for(state=0; state < 4; state++) {
	  *util_v += statd[state] * x[state] * y[state] * z[state];
        }
	x += 4;
	y += 4;
	z += 4;
        util_v++;
      }
    }
    else if(n_states == 20) {
      for(site=0; site < n_sites; site++) {
        for(state=0; state < 20; state++) {
          *util_v += statd[state] * x[state] * y[state] * z[state];
        }
	x += 20;
	y += 20;
	z += 20;
        util_v++;
      }
    }
    else {
      for(site=0; site < n_sites; site++) {
        for(state=0; state < n_states; state++) {
          *util_v += statd[state] * (*x) * (*y) * (*z);
          x++; y++; z++;
        }
        util_v++;
      }
    }
  }
  // now total up the likes from the util vector
  double ll_tot=0;
  for(site=0; site < n_sites; site++) {
    ll_tot += log(util[site]);
  }
  // subtract once rather than perform division by n_rates n_sites times
  ll_tot -= ((float) n_sites) * log ((float) n_rates);
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value pairwise_prod_c(value dst_value, value x_value, value y_value)
{
  CAMLparam3(dst_value, x_value, y_value);
  double *dst = Data_bigarray_val(dst_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int size = 
    (Bigarray_val(x_value)->dim[0]) 
    * (Bigarray_val(x_value)->dim[1])
    * (Bigarray_val(x_value)->dim[2]);
  int i;
  for(i=0; i < size; i++) {
    dst[i] = x[i] * y[i];
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

/* stores a times b^T in dst */
CAMLprim value gemmish_c(value dst_value, value a_value, value b_value)
{
  CAMLparam3(dst_value, a_value, b_value);
  double *dst = Data_bigarray_val(dst_value);
  double *a = Data_bigarray_val(a_value);
  double *b = Data_bigarray_val(b_value);
  int n_states = Bigarray_val(a_value)->dim[0];
  int n_sites = Bigarray_val(b_value)->dim[0];
  int site, i, j;
  double *a_start = a;
  if( n_states != Bigarray_val(a_value)->dim[1] ||
      n_sites  != Bigarray_val(dst_value)->dim[0] ||
      n_states != Bigarray_val(dst_value)->dim[1] ) {
    printf("bad input dimensions!");
  };
  if(n_states == 4) {
    for(site=0; site < n_sites; site++) { 
      // start back at the top of the matrix
      a = a_start;
      // going down the matrix a and across dst
      for(i=0; i < 4; i++) { 
        *dst = 0;
        // going across a
        for(j=0; j < 4; j++) { *dst += a[j] * b[j]; }
	a += 4;
        dst++;
      }
      b += n_states;
    }
  }
  else if(n_states == 20) {
    for(site=0; site < n_sites; site++) { 
      // start back at the top of the matrix
      a = a_start;
      // going down the matrix a and across dst
      for(i=0; i < 20; i++) { 
        *dst = 0;
        // going across a
        for(j=0; j < 20; j++) { *dst += a[j] * b[j]; }
	a += 20;
        dst++;
      }
      b += n_states;
    }
  }
  else {
    for(site=0; site < n_sites; site++) { 
      // start back at the top of the matrix
      a = a_start;
      // going down the matrix a and across dst
      for(i=0; i < n_states; i++) { 
        *dst = 0;
        // going across a
        for(j=0; j < n_states; j++) { *dst += a[j] * b[j]; }
	a += n_states;
        dst++;
      }
      b += n_states;
    }
  }
  CAMLreturn(Val_unit);
}
