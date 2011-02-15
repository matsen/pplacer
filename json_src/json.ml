open Jsontype

let of_string s =
  let lexbuf = Lexing.from_string s in
  Jsonparse.parse Jsonlex.token lexbuf

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
  let pf = Format.printf in
  let _ = match o with
    | Bool b -> pf "%s" (string_of_bool b)
    | Int i -> pf "%d" i
    | Float f -> pf "%f" f
    | String s -> pf "\"%s\"" (quote s)
    | Object o ->
      pf "{@[@,";
      let _ = Hashtbl.fold (fun k v is_first ->
        if not is_first then pf ",@ ";
        pf "\"%s\":@ " k;
        to_formatter ff v;
        false
      ) o true in ();
      pf "@,@]}"
    | Array o ->
      pf "[@[@,";
      let _ = Array.fold_left (fun is_first o ->
        if not is_first then pf ",@ ";
        to_formatter ff o;
        false
      ) true o in ();
      pf "@,@]]"
    | Null -> pf "null"
  in Format.pp_print_flush ff ()

let to_string o =
  let buf = Buffer.create 256 in
  to_formatter (Format.formatter_of_buffer buf) o;
  Buffer.contents buf

