open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit refpkg_cmd ~required:true as super_refpkg

  method specl = super_out_prefix#specl @ super_refpkg#specl

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
    end
end
