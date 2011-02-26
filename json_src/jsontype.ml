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

let name = function
  | Bool _ -> "bool"
  | Int _
  | Float _ -> "number"
  | String _ -> "string"
  | Object _ -> "object"
  | Array _ -> "array"
  | Null -> "null"

exception Invalid_format of string
let unexpected got expected =
  raise (Invalid_format (Printf.sprintf "expected %s, got %s" expected (name got)))

let bool = function
  | Bool b -> b
  | x -> unexpected x "bool"

let int = function
  | Int i -> i
  | Float f -> int_of_float f
  | x -> unexpected x "int"

let float = function
  | Float f -> f
  | Int i -> float_of_int i
  | x -> unexpected x "float"

let string = function
  | String s -> s
  | x -> unexpected x "string"

let obj = function
  | Object o -> o
  | x -> unexpected x "object"

let array = function
  | Array a -> a
  | x -> unexpected x "array"

let null = function
  | Null -> ()
  | x -> unexpected x "null"
