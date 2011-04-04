open Subcommand
open Guppy_cmdobjs

let index_of l x =
  let rec aux accum = function
    | x' :: rest ->
      if x = x' then accum
      else aux (accum + 1) rest
    | [] -> raise Not_found
  in aux 0 l

let gap_regexp = Str.regexp "-"
let taxid_regexp = Str.regexp "_"

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit refpkg_cmd ~required:true as super_refpkg

  val included_ranks = flag "-r"
   (Plain ([], "Ranks to include in the annotated fasta file. Can be comma-separated."))

  method specl =
    super_out_prefix#specl
  @ super_refpkg#specl
  @ [string_list_flag included_ranks]

  method desc = "convert a reference package to a format RDP wants"
  method usage = "usage: to_rdp -c my.refpkg -o prefix"

  method action _ =
    let included = List.map
      int_of_string
      (List.flatten
         (List.map
            (Str.split (Str.regexp ","))
            (fv included_ranks)))
    and rp = self#get_rp in
    let tax = Refpkg.get_taxonomy rp in
    let is_included tid = List.mem (Tax_taxonomy.get_tax_rank tax tid) included
    in
    begin
      let out_ch = open_out ((fv out_prefix) ^ "queryseq.txt") in
      let taxonomy = Tax_id.TaxIdMap.fold
        (fun tax_id name taxl ->
          if is_included tax_id then
            (Tax_taxonomy.get_tax_rank tax tax_id, tax_id, name) :: taxl
          else
            taxl)
        tax.Tax_taxonomy.tax_name_map
        []
      in
      let taxonomy = List.sort (fun (a, _, _) (b, _, _) -> a - b) taxonomy in
      List.iter
        (fun (_, tax_id, name) ->
          let ancestor =
            try
              Tax_id.to_string (List.find
                (fun tid ->
                  tax_id != tid && is_included tid)
                (List.rev (Tax_taxonomy.get_lineage tax tax_id)))
            with
              | Not_found -> "0"
          in
          Printf.fprintf out_ch
            "%s*%s*%s*%d*%s\n"
            (* oh god this is terrible *)
            (Str.global_replace
               taxid_regexp
               "00"
               (Tax_id.to_string tax_id))
            name
            ancestor
            (index_of included (Tax_taxonomy.get_tax_rank tax tax_id))
            (Tax_taxonomy.rank_name_of_tax_id tax tax_id))
        taxonomy;
      close_out out_ch
    end;
    begin
      let seqinfo = Refpkg.get_seqinfom rp
      and aln = Refpkg.get_aln_fasta rp
      and tax = Refpkg.get_taxonomy rp
      and out_ch = open_out ((fv out_prefix) ^ "queryseq.fasta") in
      Array.iter
        (fun (name, seq) ->
          let tax_id = Tax_seqinfo.tax_id_by_name seqinfo name
          and seq = Str.global_replace gap_regexp "" seq in
          Printf.fprintf out_ch
            ">%s %s\n%s\n"
            name
            (String.concat ""
               (List.rev (List.fold_left
                  (fun l tax_id ->
                    if is_included tax_id
                    then ((Tax_taxonomy.get_tax_name tax tax_id) ^ ";") :: l
                    else l)
                  []
                  (Tax_taxonomy.get_lineage tax tax_id))))
            seq)
        aln;
      close_out out_ch
    end
end
