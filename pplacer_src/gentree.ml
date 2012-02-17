open Ppatteries

let () =
  let a = float_of_string Sys.argv.(1)
  and b = float_of_string Sys.argv.(2)
  and size = int_of_string Sys.argv.(3)
  and r = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
  open_in_bin "/dev/random"
    |> with_dispose
        ~dispose:close_in
        (IO.read_i64 |- Int64.to_nativeint |- Gsl_rng.set r);
  let t = Commiesim.generate_lengthy_tree r ~a ~b size in
  if Array.length Sys.argv = 5 then
    Newick_gtree.to_file t Sys.argv.(4)
  else
    let name_fmt = Scanf.format_from_string Sys.argv.(4) "%d"
    and mkpr = Commiesim.write_random_lengthy_pr r t in
    Array.sub Sys.argv 5 (Array.length Sys.argv - 5)
      |> Array.map int_of_string
      |> Array.iter (fun n -> mkpr (Printf.sprintf name_fmt n) n)
