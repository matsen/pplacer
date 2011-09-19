type jsontype =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Object of (string, jsontype) Hashtbl.t
  | Array of jsontype list
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
