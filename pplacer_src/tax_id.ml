(* The taxonomic id type
 *
 * note use of Pervasives.compare should be redone if speed needed.
*)

exception UnknownTaxIDPrefix of char

type tax_id = NCBI of string | NoTax

let none_str = "none"

(* *** utility *** *)
let ncbi_of_stro = function
  | Some s -> NCBI s
  | None -> NoTax

let to_str = function
  | NCBI s -> "N*"^s
  | NoTax -> none_str

let to_bare_str = function
  | NCBI s -> s
  | NoTax -> none_str

let of_string id_str =
  if id_str = none_str then NoTax
  else
    try
      Scanf.sscanf id_str "%c*%s"
        (fun c s ->
          match c with
          | 'N' -> NCBI s
          | _ -> raise (UnknownTaxIDPrefix c))
    with
    | End_of_file -> invalid_arg (id_str^" is not a valid tax id!")

(* *** I/O *** *)
let ppr ff ti =
  Format.pp_print_string ff (to_str ti)

let to_xml = function
  | NCBI s -> [Myxml.tag "id" ~attributes:[("provider", "ncbi_taxonomy")] s]
  | NoTax -> []

let write_xml ch = function
  | NCBI s -> Printf.fprintf ch "<id provider=\"ncbi_taxonomy\">%s</id>\n" s
  | NoTax -> ()


(* *** Maps and Sets *** *)
module OrderedTaxId = struct
  type t = tax_id
  let compare = Pervasives.compare
end

module StringableTaxId = struct
  type t = tax_id
  let to_string = to_str
end

module TaxIdMap = Map.Make(OrderedTaxId)
module TaxIdMapFuns = MapsSets.MapFuns (OrderedTaxId) (StringableTaxId)
module TaxIdSet = Set.Make(OrderedTaxId)
module TaxIdSetFuns = MapsSets.SetFuns (OrderedTaxId) (StringableTaxId)

