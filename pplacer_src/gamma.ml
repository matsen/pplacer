(* i consulted Yang's tools.c from PAML in writing this code
 *
 * the GSL gamma is computed according to
 *   p(x) dx = {1 \over \Gamma(a) b^a} x^{a-1} e^{-x/b} dx
 * so a is the shape parameter, and b is the scale parameter.
 * on wikipedia, a is k and b is theta.
 *
 * Yang works in terms of alpha and beta, where alpha is the shape parameter,
 * and beta is the rate parameter, which is one over the scale parameter.
 *)

open Fam_batteries

(* THIS IS NOT RIGHT-- but it runs. *)
let inverse_gamma_cdf ~alpha ~beta x =
  if alpha < 0.1 then 0.
  else Gsl_cdf.gamma_Pinv ~p:x ~a:alpha ~b:(1. /. beta)

(* THIS IS THE RIGHT ONE-- but it doesn't catch the exception. *)
let correct_inverse_gamma_cdf ~alpha ~beta x =
  try Gsl_cdf.gamma_Pinv ~p:x ~a:alpha ~b:(1. /. beta) with
  | Gsl_error.Gsl_exn(_, _) ->
    if alpha < 0.1 then 0. (* alpha is small and we are close to zero. *)
    else failwith "inverse_gamma_cdf failed in a bizarre way"

let make_mean_one v =
  let tot = Array.fold_left (+.) 0. v
  and f_n_entries = float_of_int (Array.length v) in
  Array.map (fun x -> f_n_entries *. x /. tot) v

(*
 * incomplete_gamma :
 * According to GSL, gamma_inc_P is the complementary normalized incomplete
 * Gamma Function is
 *   P(a,x) = 1/\Gamma(a) \int_0^x dt t^{a-1} \exp(-t) for a > 0, x >= 0.
 * which is what Yang calls the incomplete gamma function.
 * We use his terminology here.
 *)
let incomplete_gamma ~alpha x = Gsl_sf.gamma_inc_P alpha x

let int_div i j = (float_of_int i) /. (float_of_int j)

(* discrete_gamma :
 * make a discrete gamma rate distribution with n_cat categories
 * agrees with Fig. 1 of Yang 1994
 # discrete_gamma 4 0.5;;
- : float array =
[0.0333877533835995541; 0.251915917593438454; 0.820268481973647878;
  2.89442784704931411]
*)
let discrete_gamma n_cat alpha =
  assert(n_cat > 0);
  let freq =
    Array.init
      (n_cat+1)
      (fun i ->
        if i=0 then 0.
        else if i=n_cat then 1.
        else begin
          (* equation (9) of Yang J Mol Evol 1994. *)
          let cutpoint =
            inverse_gamma_cdf (int_div i n_cat) ~alpha:alpha ~beta:1. in
          (* equation (10) of Yang J Mol Evol 1994 pre subtraction *)
          (* the +1 for alpha is so that we are getting the integral of x times
           * the gamma function *)
          incomplete_gamma cutpoint ~alpha:(alpha +. 1.)
        end)
  in
  make_mean_one (Array.init n_cat (fun i -> freq.(i+1) -. freq.(i)))

