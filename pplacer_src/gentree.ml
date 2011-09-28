open Ppatteries

let () =
  let a = float_of_string Sys.argv.(1)
  and b = float_of_string Sys.argv.(2)
  and size = int_of_string Sys.argv.(3)
  and name = Sys.argv.(4)
  and n_pqueries = int_of_string Sys.argv.(5)
  and r = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
  open_in_bin "/dev/random"
    |> with_dispose
        ~dispose:close_in
        (IO.read_i64 |- Int64.to_nativeint |- Gsl_rng.set r);
  let t = Commiesim.generate_lengthy_tree r ~a ~b size in
  Commiesim.write_random_lengthy_pr r t name n_pqueries
