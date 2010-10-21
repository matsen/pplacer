open Json

exception Type_mismatch

let get_bool = function | Bool b -> b | _ -> raise Type_mismatch
let get_int = function | Int i -> i | _ -> raise Type_mismatch
let get_float = function | Float i -> i | _ -> raise Type_mismatch
let get_string = function | String s -> s | _ -> raise Type_mismatch
let get_hashtbl = function | Object h -> h | _ -> raise Type_mismatch
let get_array = function | Array a -> a | _ -> raise Type_mismatch

let full = " { \"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25, \"address\": { \"streetAddress\": \"21 2nd Street\", \"city\": \"New York\", \"state\": \"NY\", \"postalCode\": \"10021\" }, \"phoneNumber\": [ { \"type\": \"home\", \"number\": \"212 555-1234\" }, { \"type\": \"fax\", \"number\": \"646 555-4567\" } ] } "
let small = " { \"firstName\": \"John\" } "

let slurp fname = String.concat " " (File_parsing.string_list_of_file fname)

let of_file fname = deserialize (slurp fname)

(*
let h = get_hashtbl (of_file "json_example.json")

let test () = Printf.printf "%g\n" (get_float (Hashtbl.find h "test"))
*)
