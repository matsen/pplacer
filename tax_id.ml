(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The taxonomic id type
 *
 * note use of Pervasives.compare should be redone if speed needed.
*)

type tax_id = NCBI of string | NoTax


(* *** utility *** *)
let ncbi_of_stro = function
  | Some s -> NCBI s
  | None -> NoTax

let to_string = function
  | NCBI s -> "ncbi_"^s
  | NoTax -> "none"


(* *** I/O *** *)
let ppr ff ti = 
  Format.pp_print_string ff (to_string ti)

let write_xml ch = function
  | NCBI s -> Printf.fprintf ch "<id provider=\"NCBI\">%s</id>" s
  | NoTax -> ()


(* *** Maps and Sets *** *)
module OrderedTaxId = struct
  type t = tax_id
  let compare = Pervasives.compare
end

module StringableTaxId = struct
  type t = tax_id
  let to_string = to_string
end

module TaxIdMap = Map.Make(OrderedTaxId)
module TaxIdMapFuns = MapsSets.MapFuns (OrderedTaxId) (StringableTaxId)
module TaxIdSet = Set.Make(OrderedTaxId)
module TaxIdSetFuns = MapsSets.SetFuns (OrderedTaxId) (StringableTaxId)

