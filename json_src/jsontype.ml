let location_of_position p = p.Lexing.pos_lnum, p.Lexing.pos_cnum - p.Lexing.pos_bol

exception Parse_error of string * (int * int) * (int * int)
let parse_error_of_positions s p1 p2 =
  Parse_error (s, location_of_position p1, location_of_position p2)

let format_error = function
  | Parse_error (msg, (l1, c1), (l2, c2)) -> Printf.sprintf "%s between %d:%d and %d:%d" msg l1 c1 l2 c2
  | _ -> raise (Invalid_argument "format_error")

type jsontype =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Object of (string, jsontype) Hashtbl.t
  | Array of jsontype array
  | Null
