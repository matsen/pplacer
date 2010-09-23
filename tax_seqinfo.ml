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
(* current headers: 
  * "seqname","accession","tax_id","tax_name","isType","ok","i","outlier","selected","label" *)
let of_csv fname = 
  match R_csv.list_list_of_file fname with
  | _::l  -> 
      List.fold_left 
        (fun sim -> function
          | (Some seqname_str)
            ::accession_stro
            ::(Some tax_id_str)
            ::_ -> begin
              let tax_id = NCBI tax_id_str in
              try
                StringMapFuns.check_add 
                  seqname_str
                  { tax_id = tax_id; accession = accession_stro }
                  sim
              with
              | Failure "check_add" ->
                  failwith 
                  (Printf.sprintf "Tax_refdata.of_csv: contradictory line for %s\n" seqname_str)
            end
          | _ -> failwith ("malformed line in "^fname^"... NA present or wrong number of fields"))
        StringMap.empty
        l
  | _ -> invalid_arg ("empty refdata: "^fname)
