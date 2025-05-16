(* The taxonomic id type
 *
 * note use of Pervasives.compare should be redone if speed needed.
*)

open Ppatteries
exception UnknownTaxIDPrefix of char

type tax_id = TaxStr of string | NoTax
type t = tax_id

let none_str = "none"

(* *** utility *** *)
let of_stro = function
  | Some s -> TaxStr s
  | None -> NoTax

let of_string = function
  | s when s = none_str -> NoTax
  | s -> TaxStr s

let to_string = function
  | TaxStr s -> s
  | NoTax -> none_str

let to_stro = function
  | TaxStr s -> Some s
  | NoTax -> None

let of_json = function
  | Jsontype.String s -> TaxStr s
  | Jsontype.Null -> NoTax
  | x -> Jsontype.unexpected x "string or null"

let to_json = function
  | TaxStr s -> Jsontype.String s
  | NoTax -> Jsontype.Null

let to_sql = function
  | TaxStr s -> Sql.D.TEXT s
  | NoTax -> Sql.D.NULL

let of_old_string id_str =
  if id_str = none_str then NoTax
  else
    try
      Scanf.sscanf id_str "%c*%s"
        (fun c s ->
          match c with
          | 'N' -> TaxStr s
          | _ -> raise (UnknownTaxIDPrefix c))
    with
    | End_of_file -> invalid_arg (id_str^" is not a valid tax id!")

(* *** I/O *** *)
let ppr ff ti =
  Format.pp_print_string ff (to_string ti)

let to_xml = function
  | TaxStr s -> [Myxml.tag "id" ~attrs:[("provider", "ncbi_taxonomy")] s]
  | NoTax -> []

let print ch x =
  Option.print String.print ch (to_stro x)

(* *** Maps and Sets *** *)
module OrderedTaxId = struct
  type t = tax_id
  let compare = Stdlib.compare
end

module PprTaxId = struct
  type t = tax_id
  let ppr = ppr
end

module TaxIdMap = BetterMap (Map.Make(OrderedTaxId)) (PprTaxId)
module TaxIdSet = BetterSet (Set.Make(OrderedTaxId)) (PprTaxId)
