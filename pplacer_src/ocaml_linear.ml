(* just the calls for linear_c.
*)

open Bigarray

let mdims m = (Array2.dim1 m, Array2.dim2 m)

let get_common f = function
  | x::l ->
      let fx = f x in
      List.iter (fun y -> assert(fx = f y)) l;
      fx
  | [] -> invalid_arg "get_common"

let get_common_mdims l = get_common mdims l
let get_common_tdims l = get_common Tensor.dims l

let pairwise_prod dst x y =
  let (n_rates, n_sites, n_states) = get_common_tdims [dst;x;y] in
  for rate=0 to n_rates-1 do
    for site=0 to n_sites-1 do
      for state=0 to n_states-1 do
        dst.{rate,site,state} <- x.{rate,site,state} *. y.{rate,site,state}
      done
    done
  done

let statd_pairwise_prod statd dst a b =
  let (n_rates, n_sites, n_states) = get_common_tdims [dst;a;b] in
  assert(n_states = Array1.dim statd);
  for rate=0 to n_rates-1 do
    for site=0 to n_sites-1 do
      for state=0 to n_states-1 do
        dst.{rate,site,state} <-
          statd.{state} *. a.{rate,site,state} *. b.{rate,site,state}
      done
    done
  done

(* total up the likes from the util vector *)
let ll_of_util util ~n_rates ~n_sites =
  let ll_tot = ref 0. in
  for site=0 to n_sites-1 do
    ll_tot := !ll_tot +. log(util.{site})
  done;
  (* subtract once rather than perform division by n_rates n_sites times *)
  ll_tot := !ll_tot -. (float_of_int n_sites) *. (log (float_of_int n_rates));
  !ll_tot

let log_like3 statd x y z util =
  let (n_rates, n_sites, n_states) = get_common_tdims [x;y;z] in
  assert(n_states <= Array1.dim util);
  for site=0 to n_sites-1 do util.{site} <- 0.0 done;
  for rate=0 to n_rates-1 do
    for site=0 to n_sites-1 do
      for state=0 to n_states-1 do
        util.{site} <-
          util.{site} +.
             statd.{state} *. x.{rate,site,state}
                           *. y.{rate,site,state}
                           *. z.{rate,site,state}
      done
    done
  done;
  ll_of_util util ~n_rates ~n_sites

(* take the logarithm of the dot product of x and y restricted to the interval
 * [first, last] (inclusive). first and last are 0-indexed, of course.
 * *)
let bounded_logdot x y first last util =
  let (n_rates, n_sites, n_states) = get_common_tdims [x;y]
  and n_used = 1 + last - first
  in
  assert(0 <= first && last > first && last <= n_sites);
  assert(n_used <= Array1.dim util);
  for site=0 to n_used-1 do util.{site} <- 0.0 done;
  for rate=0 to n_rates-1 do
    for site=first to last do
      for state=0 to n_states-1 do
        let util_site = site-first in
        util.{util_site} <-
          util.{util_site} +. x.{rate,site,state} *. y.{rate,site,state}
      done
    done
  done;
  ll_of_util util ~n_rates ~n_sites:n_used

let gemmish dst a b =
  let (a_rows,a_cols) = mdims a
  and (n_sites, n_states) = get_common_mdims [dst;b]
  in
  assert(a_rows = a_cols);
  assert(a_rows = n_states);
  for site=0 to n_sites-1 do
    for dst_col=0 to n_states-1 do
      dst.{site,dst_col} <- 0.;
      for i=0 to n_states-1 do
        dst.{site,dst_col} <- dst.{site,dst_col} +. a.{dst_col,i} *. b.{site,i}
      done
    done
  done

let dediagonalize dst u lambda uit =
  let n = Gsl.Vector.length lambda in
  for i=0 to n-1 do
    for j=0 to n-1 do
      dst.{i,j} <- 0.;
      for k=0 to n-1 do
        dst.{i,j} <- dst.{i,j} +. lambda.{k} *. u.{i,k} *. uit.{j,k}
      done
    done
  done
