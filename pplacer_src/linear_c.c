/* 
 * to understand the pointer arithmetic below, it's important to understand
 * the layout of the Glv's. they are row-major and indexed in terms of rate,
 * then site, then state. thus the rate-blocks are n_sites*n_states in size.
 *
 * Also, the c_layout used by bigarray means that by incrementing a pointer,
 * we first go across the array in the farthest right index, then increment
 * the next index when we go past the end on that index. E.g., we go from
 * (i,n-1) to (i+1,0).
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
  CAMLparam5(statd_value, x_value, y_value, z_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *statd = Data_bigarray_val(statd_value);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  double *z = Data_bigarray_val(z_value);
  double *util = Data_bigarray_val(util_value);
  int n_rates = Bigarray_val(x_value)->dim[0];
  int n_sites = Bigarray_val(x_value)->dim[1];
  int n_states = Bigarray_val(x_value)->dim[2];
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

CAMLprim value statd_pairwise_prod_c(value statd_value, value dst_value, value a_value, value b_value)
{
  CAMLparam4(statd_value, dst_value, a_value, b_value);
  double *statd = Data_bigarray_val(statd_value);
  double *dst = Data_bigarray_val(dst_value);
  double *a = Data_bigarray_val(a_value);
  double *b = Data_bigarray_val(b_value);
  int n_rates = Bigarray_val(a_value)->dim[0];
  int n_sites = Bigarray_val(a_value)->dim[1];
  int n_states = Bigarray_val(a_value)->dim[2];
  int rate, site, state;
  for(rate=0; rate < n_rates; rate++) {
      for(site=0; site < n_sites; site++) {
        for(state=0; state < n_states; state++) {
          *dst = statd[state] * (*a) * (*b);
          dst++; a++; b++;
	}
      }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value bounded_logdot_c(value x_value, value y_value, value first_value, value last_value, value util_value)
{
  CAMLparam5(x_value, y_value, first_value, last_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *x = Data_bigarray_val(x_value);
  double *y = Data_bigarray_val(y_value);
  int first = Int_val(first_value);
  int last = Int_val(last_value);
  double *util = Data_bigarray_val(util_value);
  int n_rates = Bigarray_val(x_value)->dim[0];
  int n_sites = Bigarray_val(x_value)->dim[1];
  int n_states = Bigarray_val(x_value)->dim[2];
  int rate, site, state;
  int n_used = 1 + last - first;
  /* we make pointers to x, y, and util so that we can do pointer arithmetic
   * and then return to where we started. */
  double *x_p, *y_p, *util_v;
  // now we clear it out to n_used
  for(site=0; site < n_used; site++) { util[site] = 0.0; }
  for(rate=0; rate < n_rates; rate++) {
    // for each rate, start at the top of the util vector
    util_v = util;
    // 1st term: gets us at top of correct rate cat
    // 2nd term: gets us "start" entries down that rate cat
    int boost = rate * n_sites * n_states + first * n_states;
    x_p = x + boost;
    y_p = y + boost;
    if(n_states == 4) {
      for(site=0; site < n_used; site++) {
        for(state=0; state < 4; state++) {
          *util_v += x_p[state] * y_p[state];
        }
        x_p += 4; y_p += 4;
        util_v++;
      }
    }
    else if(n_states == 20) {
      for(site=0; site < n_used; site++) {
        for(state=0; state < 20; state++) {
          *util_v += x_p[state] * y_p[state];
        }
        x_p += 20; y_p += 20;
        util_v++;
      }
    }
    else {
      for(site=0; site < n_used; site++) {
        for(state=0; state < n_states; state++) {
          *util_v += x_p[state] * y_p[state];
        }
        x_p += n_states; y_p += n_states;
        util_v++;
      }
    }
  }
  // now total up the likes from the util vector
  double ll_tot=0;
  for(site=0; site < n_used; site++) {
    ll_tot += log(util[site]);
  }
  // subtract once rather than perform division by n_rates n_sites times
  ll_tot -= ((float) n_used) * log ((float) n_rates);
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value dediagonalize (value dst_value, value u_value, value lambda_value, value uit_value)
{
  CAMLparam4(dst_value, u_value, lambda_value, uit_value);
  double *dst = Data_bigarray_val(dst_value);
  double *u = Data_bigarray_val(u_value);
  double *lambda = Data_bigarray_val(lambda_value);
  double *uit = Data_bigarray_val(uit_value);
  double *uit_p;
  int n = Bigarray_val(lambda_value)->dim[0];
  int i, j, k;
  /* dst.{i,j} <- dst.{i,j} +. (lambda.{k} *. u.{i,k} *. uit.{j,k}) */
  if(n == 4) {
    for(i=0; i < 4; i++) {
      uit_p = uit;
      for(j=0; j < 4; j++) {
        *dst = 0;
        for(k=0; k < 4; k++) {
          *dst += lambda[k] * u[k] * uit_p[k];
        }
        dst++; // dst.{i,j}
        uit_p += 4; // uit.{j,k}
      }
      u += 4; // u.{i,k}
    }
  }
  else if(n == 20) {
    for(i=0; i < 20; i++) {
      uit_p = uit;
      for(j=0; j < 20; j++) {
        *dst = 0;
        for(k=0; k < 20; k++) {
          *dst += lambda[k] * u[k] * uit_p[k];
        }
        dst++; // dst.{i,j}
        uit_p += 20; // uit.{j,k}
      }
      u += 20; // u.{i,k}
    }
  }
  else {
    for(i=0; i < n; i++) {
      uit_p = uit;
      for(j=0; j < n; j++) {
        *dst = 0;
        for(k=0; k < n; k++) {
          *dst += lambda[k] * u[k] * uit_p[k];
        }
        dst++; // dst.{i,j}
        uit_p += n; // uit.{j,k}
      }
      u += n; // u.{i,k}
    }
  }
  CAMLreturn(Val_unit);
}

