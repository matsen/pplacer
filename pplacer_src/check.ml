(* routines for doing sanity checks *)

open Ppatteries

(* check that dir_name is actually a directory *)
let directory dir_name =
  try
    if not (Sys.is_directory dir_name) then
      raise (Sys_error "")
  with
  | Sys_error _ ->
      failwith
        (Printf.sprintf "Bad directory specification: '%s'"
        dir_name)
