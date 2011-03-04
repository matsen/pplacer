(* just the calls for linear_c.
*)

open Bigarray

let get_common f = function
  | x::l ->
      let fx = f x in
      List.iter (fun y -> assert(fx = f y)) l;
      fx
  | [] -> assert(false)

let get_common_dims l = get_common Tensor.dims l

let pairwise_prod dst x y =
  let (n_rates, n_sites, n_states) = get_common_dims [dst;x;y] in
  for rate=0 to n_rates-1 do
    for site=0 to n_sites-1 do
      for state=0 to n_states-1 do
        dst.{rate,site,state} <- x.{rate,site,state} *. y.{rate,site,state}
      done
    done
  done

let statd_pairwise_prod statd dst a b =
  let (n_rates, n_sites, n_states) = get_common_dims [dst;a;b] in
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
let ll_of_util util n_rates =
  let n_sites = Array1.dim util
  and ll_tot = ref 0.
  in
  for site=0 to n_sites-1 do
    ll_tot := !ll_tot +. log(util.{site})
  done;
  (* subtract once rather than perform division by n_rates n_sites times *)
  ll_tot := !ll_tot -. (float_of_int n_sites) *. (log (float_of_int n_rates));
  !ll_tot

let log_like3 statd x y z util =
  let (n_rates, n_sites, n_states) = get_common_dims [x;y;z] in
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
  ll_of_util util n_rates

(* take the logarithm of the dot product of x and y restricted to the interval
 * [start, last]. start and last are 0-indexed, of course.
 * *)
let bounded_logdot x y first last util =
  let (n_rates, _, n_states) = get_common_dims [x;y]
  and n_used = 1 + last - first
  in
  for site=0 to n_used-1 do util.{site} <- 0.0 done;
  for rate=0 to n_rates-1 do
    for site=first to last-1 do
      for state=0 to n_states-1 do
        let util_site = site-first in
        util.{util_site} <-
          util.{util_site} +. x.{rate,site,state} *. y.{rate,site,state}
      done
    done
  done;
  ll_of_util util n_rates

let gemmish dst a b =
  let (max_i,max_k) = Gsl_matrix.dims a
  and (max_k',max_j) = Gsl_matrix.dims b
  in
  assert(max_k = max_k');
  assert((max_i,max_j) = Gsl_matrix.dims dst);
  for i=0 to max_i-1 do
    for j=0 to max_j-1 do
      dst.{i,j} <- 0.;
      for k=0 to max_k-1 do
        dst.{i,j} <- dst.{i,j} +. a.{i,k} *. b.{k,j}
      done
    done
  done

let dediagonalize dst u lambda uit =
  let n = Gsl_vector.length lambda in
  for i=0 to n-1 do
    for j=0 to n-1 do
      dst.{i,j} <- 0.;
      for k=0 to n-1 do
        dst.{i,j} <- dst.{i,j} +. lambda.{k} *. u.{i,k} *. uit.{j,k}
      done
    done
  done
