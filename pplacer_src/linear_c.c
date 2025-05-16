/*
 * Roll up your bell-bottoms, boys and girls, it's time for some retro pointer
 * arithmetic!
 *
 * To understand the pointer arithmetic below, it's important to understand
 * the layout of the Glv's. they are row-major and indexed in terms of rate,
 * then site, then state. Thus the rate-blocks are n_sites*n_states in size.
 *
 * Also, the c_layout used by bigarray means that by incrementing a pointer,
 * we first go across the array in the farthest right index, then increment
 * the next index when we go past the end on that index. E.g., we go from
 * (i,n-1) to (i+1,0).
*/

#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>


/* *** Matrices, used for transformation. *** */

/* stores a times b^T in dst */
CAMLprim value gemmish_c(value dst_value, value a_value, value b_value)
{
  CAMLparam3(dst_value, a_value, b_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *a = Caml_ba_data_val(a_value);
  double *b = Caml_ba_data_val(b_value);
  int n_states = Caml_ba_array_val(a_value)->dim[0];
  int n_sites = Caml_ba_array_val(b_value)->dim[0];
  int site, i, j;
  double *a_start = a;
  if( n_states != Caml_ba_array_val(a_value)->dim[1] ||
      n_sites  != Caml_ba_array_val(dst_value)->dim[0] ||
      n_states != Caml_ba_array_val(dst_value)->dim[1] ) {
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

CAMLprim value dediagonalize (value dst_value, value u_value, value lambda_value, value uit_value)
{
  CAMLparam4(dst_value, u_value, lambda_value, uit_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *u = Caml_ba_data_val(u_value);
  double *lambda = Caml_ba_data_val(lambda_value);
  double *uit = Caml_ba_data_val(uit_value);
  double *uit_p;
  int n = Caml_ba_array_val(lambda_value)->dim[0];
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



/* *** Matrices, that are used for gcat. *** */

CAMLprim value mat_print_c(value x_value)
{
  CAMLparam1(x_value);
  double *x = Caml_ba_data_val(x_value);
  int n_sites = (Caml_ba_array_val(x_value)->dim[0]);
  int n_states = (Caml_ba_array_val(x_value)->dim[1]);
  double *loc = x;
  int site, state;
  for(site=0; site < n_sites; site++) {
    for(state=0; state < n_states; state++) {
      printf("%g\t",*loc);
      loc++;
    }
    printf("\n");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value mat_log_like3_c(value statd_value, value x_value, value y_value, value z_value)
{
  CAMLparam4(statd_value, x_value, y_value, z_value);
  CAMLlocal1(ml_ll_tot);
  double *statd = Caml_ba_data_val(statd_value);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  double *z = Caml_ba_data_val(z_value);
  int n_sites = Caml_ba_array_val(x_value)->dim[0];
  int n_states = Caml_ba_array_val(x_value)->dim[1];
  int site, state;
  double util, ll_tot=0;
  // here we hard code in the limits for some popular choices
  // so that loops can get unrolled
  if(n_states == 4) {
    for(site=0; site < n_sites; site++) {
      util=0;
      for(state=0; state < 4; state++) {
        util += statd[state] * x[state] * y[state] * z[state];
      }
      ll_tot += log(util);
      x += 4; y += 4; z += 4;
    }
  }
  else if(n_states == 20) {
    for(site=0; site < n_sites; site++) {
      util=0;
      for(state=0; state < 20; state++) {
        util += statd[state] * x[state] * y[state] * z[state];
      }
      ll_tot += log(util);
      x += 20; y += 20; z += 20;
    }
  }
  else {
    for(site=0; site < n_sites; site++) {
      util=0;
      for(state=0; state < n_states; state++) {
        util += statd[state] * (*x) * (*y) * (*z);
        x++; y++; z++;
      }
      ll_tot += log(util);
    }
  }
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value mat_pairwise_prod_c(value dst_value, value x_value, value y_value)
{
  CAMLparam3(dst_value, x_value, y_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  int size = (Caml_ba_array_val(x_value)->dim[0]) * (Caml_ba_array_val(x_value)->dim[1]);
  int i;
  for(i=0; i < size; i++) {
    dst[i] = x[i] * y[i];
  }
  CAMLreturn(Val_unit);
}

CAMLprim value mat_statd_pairwise_prod_c(value statd_value, value dst_value, value a_value, value b_value)
{
  CAMLparam4(statd_value, dst_value, a_value, b_value);
  double *statd = Caml_ba_data_val(statd_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *a = Caml_ba_data_val(a_value);
  double *b = Caml_ba_data_val(b_value);
  int n_sites = Caml_ba_array_val(a_value)->dim[0];
  int n_states = Caml_ba_array_val(a_value)->dim[1];
  int site, state;
  for(site=0; site < n_sites; site++) {
    for(state=0; state < n_states; state++) {
      *dst = statd[state] * (*a) * (*b);
      dst++; a++; b++;
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value mat_masked_logdot_c(value x_value, value y_value, value mask_value)
{
  CAMLparam3(x_value, y_value, mask_value);
  CAMLlocal1(ml_ll_tot);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  uint16_t *mask = Caml_ba_data_val(mask_value);
  int n_sites = (Caml_ba_array_val(x_value)->dim[0]);
  int n_states = (Caml_ba_array_val(x_value)->dim[1]);
  if(n_sites != Caml_ba_array_val(mask_value)->dim[0])
    { printf("mat_masked_logdot_c: Mask length doesn't match!"); };
  int site, state;
  double util, ll_tot=0;
  if(n_states == 4) {
    for(site=0; site < n_sites; site++) {
      if(mask[site]) {
        util=0;
        for(state=0; state < 4; state++) { util += x[state] * y[state]; }
        ll_tot += log(util);
      }
      x += n_states; y += n_states;
    }
  }
  else if(n_states == 20) {
    for(site=0; site < n_sites; site++) {
      if(mask[site]) {
        util=0;
        for(state=0; state < 20; state++) { util += x[state] * y[state]; }
        ll_tot += log(util);
      }
      x += n_states; y += n_states;
    }
  }
  else {
    for(site=0; site < n_sites; site++) {
      if(mask[site]) {
        util=0;
        for(state=0; state < n_states; state++) { util += x[state] * y[state]; }
        ll_tot += log(util);
      }
      x += n_states; y += n_states;
    }
  }
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value mat_bounded_logdot_c(value x_value, value y_value, value first_value, value last_value)
{
  CAMLparam4(x_value, y_value, first_value, last_value);
  CAMLlocal1(ml_ll_tot);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  int first = Int_val(first_value);
  int last = Int_val(last_value);
  int n_used = 1 + last - first;
  int n_states = Caml_ba_array_val(x_value)->dim[1];
  int site, state;
  double util, ll_tot=0;
  // start at the beginning
  x += first * n_states;
  y += first * n_states;
  // calculate
  if(n_states == 4) {
    for(site=0; site < n_used; site++) {
      util=0;
      for(state=0; state < 4; state++) { util += x[state] * y[state]; }
      x += n_states; y += n_states;
      ll_tot += log(util);
    }
  }
  else if(n_states == 20) {
    for(site=0; site < n_used; site++) {
      util=0;
      for(state=0; state < 20; state++) { util += x[state] * y[state]; }
      x += n_states; y += n_states;
      ll_tot += log(util);
    }
  }
  else {
    for(site=0; site < n_used; site++) {
      util=0;
      for(state=0; state < n_states; state++) { util += x[state] * y[state]; }
      x += n_states; y += n_states;
      ll_tot += log(util);
    }
  }
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}


/* *** Tensors, that are used for gmix. *** */

CAMLprim value ten_print_c(value x_value)
{
  CAMLparam1(x_value);
  double *x = Caml_ba_data_val(x_value);
  int n_rates = (Caml_ba_array_val(x_value)->dim[0]);
  int n_sites = (Caml_ba_array_val(x_value)->dim[1]);
  int n_states = (Caml_ba_array_val(x_value)->dim[2]);
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

CAMLprim value ten_log_like3_c(value statd_value, value x_value, value y_value, value z_value, value util_value)
{
  CAMLparam5(statd_value, x_value, y_value, z_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *statd = Caml_ba_data_val(statd_value);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  double *z = Caml_ba_data_val(z_value);
  double *util = Caml_ba_data_val(util_value);
  int n_rates = Caml_ba_array_val(x_value)->dim[0];
  int n_sites = Caml_ba_array_val(x_value)->dim[1];
  int n_states = Caml_ba_array_val(x_value)->dim[2];
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
        x += 4; y += 4; z += 4;
        util_v++;
      }
    }
    else if(n_states == 20) {
      for(site=0; site < n_sites; site++) {
        for(state=0; state < 20; state++) {
          *util_v += statd[state] * x[state] * y[state] * z[state];
        }
        x += 20; y += 20; z += 20;
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

CAMLprim value ten_pairwise_prod_c(value dst_value, value x_value, value y_value)
{
  CAMLparam3(dst_value, x_value, y_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  int size =
    (Caml_ba_array_val(x_value)->dim[0])
    * (Caml_ba_array_val(x_value)->dim[1])
    * (Caml_ba_array_val(x_value)->dim[2]);
  int i;
  for(i=0; i < size; i++) {
    dst[i] = x[i] * y[i];
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ten_statd_pairwise_prod_c(value statd_value, value dst_value, value a_value, value b_value)
{
  CAMLparam4(statd_value, dst_value, a_value, b_value);
  double *statd = Caml_ba_data_val(statd_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *a = Caml_ba_data_val(a_value);
  double *b = Caml_ba_data_val(b_value);
  int n_rates = Caml_ba_array_val(a_value)->dim[0];
  int n_sites = Caml_ba_array_val(a_value)->dim[1];
  int n_states = Caml_ba_array_val(a_value)->dim[2];
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

CAMLprim value ten_masked_logdot_c(value x_value, value y_value, value mask_value, value util_value)
{
  CAMLparam4(x_value, y_value, mask_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  uint16_t *mask = Caml_ba_data_val(mask_value);
  double *util = Caml_ba_data_val(util_value);
  int n_rates = Caml_ba_array_val(x_value)->dim[0];
  int n_sites = Caml_ba_array_val(x_value)->dim[1];
  int n_states = Caml_ba_array_val(x_value)->dim[2];
  if(n_sites != Caml_ba_array_val(mask_value)->dim[0])
    { printf("ten_masked_logdot_c: Mask length doesn't match!"); };
  if(n_sites != Caml_ba_array_val(util_value)->dim[0])
    { printf("ten_masked_logdot_c: Util length doesn't match!"); };
  int rate, site, state;
  double *x_p, *y_p, *util_v;

  // Util will be used to accumulate results across rates.
  for(site=0; site < n_sites; site++) { util[site] = 0.0; }
  for(rate=0; rate < n_rates; rate++) {
    // for each rate, start at the top of the util vector
    util_v = util;
    // and start at the appropriate place in x and y
    x_p = x + rate * n_sites * n_states;
    y_p = y + rate * n_sites * n_states;
    if(n_states == 4) {
      for(site=0; site < n_sites; site++) {
        if(mask[site]) {
          for(state=0; state < 4; state++) {
            *util_v += x_p[state] * y_p[state];
          }
        }
        x_p += n_states; y_p += n_states;
        util_v++;
      }
    }
    else if(n_states == 20) {
      for(site=0; site < n_sites; site++) {
        if(mask[site]) {
          for(state=0; state < 20; state++) {
            *util_v += x_p[state] * y_p[state];
          }
        }
        x_p += n_states; y_p += n_states;
        util_v++;
      }
    }
    else {
      for(site=0; site < n_sites; site++) {
        if(mask[site]) {
          for(state=0; state < n_states; state++) {
            *util_v += x_p[state] * y_p[state];
          }
        }
        x_p += n_states; y_p += n_states;
        util_v++;
      }
    }
  }

  // now total up the likes from the util vector
  double ll_tot=0;
  int n_used=0;
  for(site=0; site < n_sites; site++) {
    if (mask[site]) {
      ll_tot += log(util[site]);
      n_used++;
    }
  }
  // subtract once rather than perform division by n_rates n_sites times
  ll_tot -= ((float) n_used) * log ((float) n_rates);
  ml_ll_tot = caml_copy_double(ll_tot);
  CAMLreturn(ml_ll_tot);
}

CAMLprim value ten_bounded_logdot_c(value x_value, value y_value, value first_value, value last_value, value util_value)
{
  CAMLparam5(x_value, y_value, first_value, last_value, util_value);
  CAMLlocal1(ml_ll_tot);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  int first = Int_val(first_value);
  int last = Int_val(last_value);
  double *util = Caml_ba_data_val(util_value);
  int n_rates = Caml_ba_array_val(x_value)->dim[0];
  int n_sites = Caml_ba_array_val(x_value)->dim[1];
  int n_states = Caml_ba_array_val(x_value)->dim[2];
  int rate, site, state;
  int n_used = 1 + last - first;
  /* we make pointers to x, y, and util so that we can do pointer arithmetic
   * and then return to where we started. */
  double *x_p, *y_p, *util_v;
  // Clear util out to n_used.
  for(site=0; site < n_used; site++) { util[site] = 0.0; }
  for(rate=0; rate < n_rates; rate++) {
    // for each rate, start at the top of the util vector
    util_v = util;
    // 1st term: gets us at top of correct rate cat
    // 2nd term: gets us "first" entries down that rate cat
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

CAMLprim value vec_pairwise_prod_c(value dst_value, value x_value, value y_value)
{
  CAMLparam3(dst_value, x_value, y_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *x = Caml_ba_data_val(x_value);
  double *y = Caml_ba_data_val(y_value);
  int i;
  for(i=0; i < Caml_ba_array_val(x_value)->dim[0]; i++) {
    dst[i] = x[i] * y[i];
  }
  CAMLreturn(Val_unit);
}

CAMLprim value int_vec_tot_c(value x_value)
{
  CAMLparam1(x_value);
  uint16_t *x = Caml_ba_data_val(x_value);
  CAMLlocal1(ml_tot);
  int i, tot = 0;
  for(i=0; i < Caml_ba_array_val(x_value)->dim[0]; i++)
    tot += *x++;
  ml_tot = Val_int(tot);
  CAMLreturn(ml_tot);
}

CAMLprim value int_vec_pairwise_prod_c(value dst_value, value x_value, value y_value)
{
  CAMLparam3(dst_value, x_value, y_value);
  uint16_t *dst = Caml_ba_data_val(dst_value);
  uint16_t *x = Caml_ba_data_val(x_value);
  uint16_t *y = Caml_ba_data_val(y_value);
  uint16_t *dst_end = dst + Caml_ba_array_val(dst_value)->dim[0];
  while (dst < dst_end)
    *dst++ = *x++ * *y++;
  CAMLreturn(Val_unit);
}

CAMLprim value float_mat_int_vec_mul_c(value dst_value, value mat_value, value vec_value)
{
  CAMLparam3(dst_value, mat_value, vec_value);
  double *dst = Caml_ba_data_val(dst_value);
  double *mat = Caml_ba_data_val(mat_value);
  uint16_t vec_j, *vec = Caml_ba_data_val(vec_value);
  int j;
  int n = Caml_ba_array_val(mat_value)->dim[0], k = Caml_ba_array_val(mat_value)->dim[1];
  double *dst_start = dst, *dst_end = dst + k;

  if (Caml_ba_array_val(vec_value)->dim[0] != n)
    caml_failwith("dim_0 vec != dim_0 mat");
  if (Caml_ba_array_val(dst_value)->dim[0] != k)
    caml_failwith("dim_0 dst != dim_1 mat");

  for (j = 0; j < n; ++j) {
    /* skip a whole row if we are multiplying by zero */
    if (!(vec_j = vec[j])) {
      mat += k;
      continue;
    }
    for (dst = dst_start; dst < dst_end;) {
      *dst++ += *mat++ * vec_j;
    }
  }
  CAMLreturn(Val_unit);
}
