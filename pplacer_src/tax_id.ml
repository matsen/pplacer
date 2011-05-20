(* The taxonomic id type
 *
 * note use of Pervasives.compare should be redone if speed needed.
*)

exception UnknownTaxIDPrefix of char

type tax_id = TaxStr of string | NoTax

let none_str = "none"

(* *** utility *** *)
let of_stro = function
  | Some s -> TaxStr s
  | None -> NoTax

let of_string = function
  | "none" -> NoTax
  | s -> TaxStr s

let to_string = function
  | TaxStr s -> s
  | NoTax -> none_str

let of_json = function
  | Jsontype.String s -> TaxStr s
  | Jsontype.Null -> NoTax
  | x -> Jsontype.unexpected x "string or null"

let to_json = function
  | TaxStr s -> Jsontype.String s
  | NoTax -> Jsontype.Null

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
  | TaxStr s -> [Myxml.tag "id" ~attributes:[("provider", "ncbi_taxonomy")] s]
  | NoTax -> []

let write_xml ch = function
  | TaxStr s -> Printf.fprintf ch "<id provider=\"ncbi_taxonomy\">%s</id>\n" s
  | NoTax -> ()


(* *** Maps and Sets *** *)
module OrderedTaxId = struct
  type t = tax_id
  let compare = Pervasives.compare
end

module PprTaxId = struct
  type t = tax_id
  let ppr = ppr
end

module TaxIdMap = MapsSets.BetterMap (Map.Make(OrderedTaxId)) (PprTaxId)
module TaxIdSet = MapsSets.BetterSet (Set.Make(OrderedTaxId)) (PprTaxId)

