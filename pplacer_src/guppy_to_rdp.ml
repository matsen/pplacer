open Subcommand
open Guppy_cmdobjs

let gap_regexp = Str.regexp "-"

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
    let rp = self#get_rp in
    let tax = Refpkg.get_taxonomy rp in
    begin
      let out_ch = open_out ((fv out_prefix) ^ "queryseq.txt") in
      Tax_id.TaxIdMap.iter
        (fun tax_id name ->
          let ancestor =
            try
              Tax_id.to_string (Tax_taxonomy.get_ancestor tax tax_id)
            with
              | Tax_taxonomy.NoAncestor _ -> "0"
          in
          Printf.fprintf out_ch
            "%s*%s*%s*%d*%s\n"
            (Tax_id.to_string tax_id)
            name
            ancestor
            (Tax_taxonomy.get_tax_rank tax tax_id)
            (Tax_taxonomy.rank_name_of_tax_id tax tax_id))
        tax.Tax_taxonomy.tax_name_map;
      close_out out_ch
    end;
    begin
      let included = List.map
        int_of_string
        (List.flatten
           (List.map
              (Str.split (Str.regexp ","))
              (fv included_ranks)))
      and seqinfo = Refpkg.get_seqinfom rp
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
               (List.fold_left
                  (fun l tax_id ->
                    if List.mem (Tax_taxonomy.get_tax_rank tax tax_id) included
                    then ((Tax_id.to_string tax_id) ^ ";") :: l
                    else l)
                  []
                  (Tax_taxonomy.get_lineage tax tax_id)))
            seq)
        aln;
      close_out out_ch
    end
end
