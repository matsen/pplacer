(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For parsing refdata files.
*)

open Tax_id
open MapsSets

type t =
  {
    tax_id    :  tax_id;
    accession :  string option;
  }

type seqinfo_map = t StringMap.t

let tax_id_by_name sim s =
  try (StringMap.find s sim).tax_id with
  | Not_found -> failwith ("tax_id for "^s^" not found!")



(* *** reading *** *)

let entry_of_str = function
  | "" -> None
  | "NA" -> None
  | s -> Some s

(* requires "seqname" and "tax_id" columns, "accession"
 * "tax_name","isType","ok","i","outlier","selected","label" *)
let of_csv fname =
  let safe_assoc k l =
    try List.assoc k l with
      | Not_found -> failwith ("couldn't find "^k^" column in "^fname)
  in
  List.fold_left
    (fun sim al ->
      let seqname_str = safe_assoc "seqname" al in
      try
        StringMapFuns.check_add
          seqname_str
          {
            tax_id = NCBI (safe_assoc "tax_id" al);
            accession =
              try entry_of_str (List.assoc "accession" al) with
              | Not_found -> None
          }
          sim
      with
      | Failure "check_add" ->
          failwith
          (Printf.sprintf "Tax_refdata.of_csv: contradictory line for %s\n" seqname_str)
      | Not_found -> failwith ("seq_name and/or tax_id fields missing in "^fname))
    StringMap.empty
    (match Csv.load fname with
    | header :: data -> Csv.associate header data
    | [] -> invalid_arg ("empty refdata: "^fname))
