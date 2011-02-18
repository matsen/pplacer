open Jsontype

exception Type_mismatch_wanted of string
exception Type_mismatch of string
exception Undefined_key of string
exception Not_object

let name_type = function
  | Bool _ -> "boolean"
  | Int _ -> "integer"
  | Float _ -> "float"
  | String _ -> "string"
  | Object _ -> "object"
  | Array _ -> "array"
  | Null -> "null"

let _get_bool = function | Bool b -> b | _ -> raise (Type_mismatch_wanted "boolean")
let _get_int = function | Int i -> i | _ -> raise (Type_mismatch_wanted "integer")
let _get_float = function | Float x -> x | _ -> raise (Type_mismatch_wanted "float")
let _get_string = function | String s -> s | _ -> raise (Type_mismatch_wanted "string")
let _get_hashtbl = function | Object h -> h | _ -> raise (Type_mismatch_wanted "object")
let _get_array = function | Array a -> a | _ -> raise (Type_mismatch_wanted "array")

let _get_real = function
  | Int i -> float_of_int i
  | Float x -> x
  | _ -> raise (Type_mismatch_wanted "real")

let slurp fname = String.concat "\n" (File_parsing.string_list_of_file fname)

let of_file fname = Json.of_string (slurp fname)

(* "getting" something simply means that we convert to a proper ocaml type. *)
let get_gen f x =
  try f x with
  | Type_mismatch_wanted wanted ->
      raise (Type_mismatch ("expected "^wanted^", got "^(name_type x)))

let get_bool    = get_gen _get_bool
let get_int     = get_gen _get_int
let get_float   = get_gen _get_float
let get_string  = get_gen _get_string
let get_hashtbl = get_gen _get_hashtbl
let get_array   = get_gen _get_array
let get_real    = get_gen _get_real

(* "finding" something means that we find it in the json object (as a
 * hashtable) and then convert to the corresponding ocaml type. *)
let find_gen what o k =
  match o with
  | Object h -> begin
      try what (Hashtbl.find h k) with
      | Not_found -> raise (Undefined_key k)
      | Type_mismatch_wanted wanted ->
          raise (Type_mismatch ("expected "^wanted^" when looking for "^k))
  end
  | _ -> raise Not_object

let find = find_gen (fun o -> o)

let find_bool    = find_gen _get_bool
let find_int     = find_gen _get_int
let find_float   = find_gen _get_float
let find_string  = find_gen _get_string
let find_hashtbl = find_gen _get_hashtbl
let find_array   = find_gen _get_array
let find_real    = find_gen _get_real

let mem o k = try let _ = find o k in true with | Undefined_key _ -> false
