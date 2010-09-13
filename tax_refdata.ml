(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For parsing refdata files.
*)

open Tax_id
open MapsSets

type seqinfo =
  {
    tax_id    :  tax_id;
    accession :  string;
  }

type refdata = 
  {
    seqinfo_map     :  seqinfo StringMap.t;
    taxid_name_map  :  string TaxIdMap.t;
  }

(* *** reading *** *)
let empty = 
  { seqinfo_map = StringMap.empty; taxid_name_map = TaxIdMap.empty }

let get_seqinfo rd seq_name = 
  try StringMap.find seq_name rd.seqinfo_map with
  | Not_found -> failwith ("Don't have info for sequence named"^seq_name)
  
let tax_id_by_name rd seq_name = (get_seqinfo rd seq_name).tax_id

let get_taxid_name rd ti = 
  try TaxIdMap.find ti rd.taxid_name_map with
  | Not_found -> failwith ("Don't know the name of taxon "^(Tax_id.to_string ti))

(* *** reading *** *)
(* current headers: 
  * "seqname","accession","tax_id","tax_name","isType","ok","i","outlier","selected","label" *)
let of_csv fname = 
  match R_csv.list_list_of_file fname with
  | _::l  -> 
      List.fold_left 
        (fun rd -> function
          | (Some seqname_str)
            ::(Some accession_str)
            ::(Some tax_id_str)
            ::(Some tax_name_str)
            ::_ -> begin
              let tax_id = NCBI tax_id_str in
              try
              { 
                seqinfo_map = 
                  StringMapFuns.check_add 
                    seqname_str
                    { tax_id = tax_id; accession = accession_str }
                    rd.seqinfo_map;
                taxid_name_map = 
                  TaxIdMapFuns.check_add 
                    tax_id 
                    tax_name_str
                    rd.taxid_name_map;
              }
              with
              | Failure "check_add" ->
                  failwith 
                  (Printf.sprintf "Tax_refdata.of_csv: contradictory %s %s %s %s\n" seqname_str accession_str tax_id_str tax_name_str)
            end
          | _ -> failwith ("malformed line in "^fname^"... NA present or wrong number of fields"))
        empty 
        l
  | _ -> invalid_arg ("empty refdata: "^fname)
