open Ppatteries

module T = Lcfit.Tripod

let evenly_spaced n min max =
  let d = max -. min
  and nf = (Float.of_int n) -. 1.0
  in
  assert(d > 0.);
  0 -- (n - 1)
  |> map Float.of_int
  |> map (fun i -> min +. (d *. i) /. nf)
  |> List.of_enum

let () =
  let args = Array.to_list Sys.argv in
  let prg = List.hd args
  and rest = List.tl args in
  let float_rest = List.map Float.of_string rest in
  match float_rest with
  | [n00; n01; n10; n11; r; b; t; rx; bx] ->
      let m = {T.n00=n00; T.n01=n01; T.n10=n10; T.n11=n11;
               T.r=r; T.b=b; T.t=t; T.rx=rx; T.bx=bx}
      in
      let log_like = T.log_like m
      and dist_bls = if t =~ 0. then [t] else evenly_spaced 10 1e-6 t
      and pend_bls = evenly_spaced 10 1e-6 2.0 in
      let pts = List.cartesian_product dist_bls pend_bls
        |> List.map (fun (x, y) -> (x, y, log_like x y))
        |> List.map (Printf.sprintf "%f,%f,%f\n" |> Tuple3.uncurry)
      in
      List.iter (Printf.printf "%s") pts
  | _ ->
      Printf.eprintf "Usage: %s n00 n01 n10 n11 r b t rx bx" prg;
      raise Exit

