open Ppatteries

let () =
  let mu = float_of_string Sys.argv.(1)
  and size = int_of_string Sys.argv.(2)
  and name = Sys.argv.(3)
  and r = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
  open_in_bin "/dev/random"
    |> with_dispose
        ~dispose:close_in
        (IO.read_i64 |- Int64.to_nativeint |- Gsl_rng.set r);
  let t = Commiesim.generate_caterpillar_tree r ~mu size in
  Placerun.make t name []
  |> Placerun_io.to_json_file "" (name ^ ".jplace")
