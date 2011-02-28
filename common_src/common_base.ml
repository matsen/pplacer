
let time_str_of_seconds secs =
  let iSecs = int_of_float secs in
  let hours = iSecs / 3600 in
  let mins = iSecs / 60 - 60*hours in
  Printf.sprintf "%02d:%02d:%02.1f" hours mins
    (secs -. (float_of_int (hours*3600)) -. (float_of_int (mins*60)))

let print_elapsed_time () =
  let t = Sys.time () in
  Printf.printf "elapsed time: %s (%g s)\n" (time_str_of_seconds t) t

let calc_total_memory () =
  let stat = Gc.quick_stat () in
  (stat.Gc.minor_words +.
   stat.Gc.major_words -.
   stat.Gc.promoted_words)
  *. (float_of_int Sys.word_size) /. 8.

let print_memory_usage () =
  Printf.printf "memory usage (bytes): %g\n" (calc_total_memory ())

let print_n_compactions () =
  Printf.printf "number of garbage compactions: %d\n"
                ((Gc.quick_stat ()).Gc.compactions)

let get_dir_contents ?(pred = (fun _ -> true)) dir_name =
  let dirh = Unix.opendir dir_name in
  let rec iter accum =
    try
      let fname = Unix.readdir dirh in
      if pred fname then iter ((dir_name ^ "/" ^ fname) :: accum)
      else iter accum
    with
      | End_of_file -> (Unix.closedir dirh; accum) in
  iter []
