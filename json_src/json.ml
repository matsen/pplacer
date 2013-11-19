open Ppatteries
open Jsontype

let of_string ?fname s =
  let lexbuf = Lexing.from_string s in
  Sparse.wrap_of_fname_opt fname (Jsonparse.parse Jsonlex.token) lexbuf

let of_file fname =
  let fobj = MaybeZipped.open_in fname in
  let input s n =
    try
      IO.input fobj s 0 n
    with BatIO.No_more_input -> 0
  in
  let lexbuf = Lexing.from_function input in
  let ret = Sparse.file_parse_wrap
    fname
    (Jsonparse.parse Jsonlex.token)
    lexbuf
  in
  IO.close_in fobj;
  ret

let to_escape = Str.regexp "\\([\\\\\"/\b\012\n\r\t]\\)"
let quote = Str.global_substitute to_escape begin fun s ->
  match Str.replace_matched "\\1" s with
    | "\\" -> "\\\\"
    | "\"" -> "\\\""
    | "/" -> "\\/"
    | "\b" -> "\\b"
    | "\012" -> "\\f"
    | "\n" -> "\\n"
    | "\r" -> "\\r"
    | "\t" -> "\\t"
    | s -> failwith ("invalid " ^ s)
end

let rec to_formatter ff o =
  let rec aux = function
    | Bool b -> Format.fprintf ff "%s" (string_of_bool b)
    | Int i -> Format.fprintf ff "%d" i
    | Float f -> Format.fprintf ff "%.12g" f
    | String s -> Format.fprintf ff "\"%s\"" (quote s)
    | Object o ->
      Format.fprintf ff "@[<2>{@,";
      let _ = Hashtbl.fold (fun k v is_first ->
        if not is_first then Format.fprintf ff ",@ ";
        Format.fprintf ff "\"%s\":@ " (quote k);
        aux v;
        false
      ) o true in ();
      Format.fprintf ff "@]@,}"
    | Array o ->
      Format.fprintf ff "@[<2>[@,";
      let _ = List.fold_left (fun is_first o ->
        if not is_first then Format.fprintf ff ",@ ";
        aux o;
        false
      ) true o in ();
      Format.fprintf ff "@]@,]"
    | Null -> Format.fprintf ff "null"
  in
  Format.fprintf ff "@[";
  aux o;
  Format.fprintf ff "@]@."

let to_string o =
  let buf = Buffer.create 256 in
  to_formatter (Format.formatter_of_buffer buf) o;
  Buffer.contents buf

let to_file name o =
  let file = MaybeZipped.open_out name in
  let formatter= Format.make_formatter
    (fun s p l -> let _ = IO.output file s p l in ())
    (fun () -> IO.flush file)
  in
  to_formatter formatter o;
  close_out file
