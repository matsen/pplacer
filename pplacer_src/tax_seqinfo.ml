(* For parsing refdata files.
*)

open Tax_id
open Ppatteries

type t =
  {
    tax_id    :  tax_id;
    accession :  string option;
  }

type seqinfo_map = t StringMap.t

let tax_id_by_node_label sim s =
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
        StringMap.check_add
          seqname_str
          {
            tax_id = safe_assoc "tax_id" al |> entry_of_str |> Tax_id.of_stro;
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
