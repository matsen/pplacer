open Subcommand
open Guppy_cmdobjs

let quote_regex = Str.regexp "'"
let escape s =
  Printf.sprintf "'%s'" (Str.global_replace quote_regex "''" s)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd () as super_refpkg
  inherit outfile_cmd () as super_outfile

  method specl =
    super_refpkg#specl
    @ super_outfile#specl

  method desc = "makes sql for a reference package"
  method usage = "usage: taxtable [options] -c <refpkg>"

  method action _ =
    let refpkg = Refpkg.of_path (fv refpkg_path) in
    let ch = self#out_channel in
    let tax = Refpkg.get_taxonomy refpkg in
    output_string ch "
      CREATE TABLE IF NOT EXISTS taxa (
        tax_id TEXT PRIMARY KEY NOT NULL,
        tax_name TEXT NOT NULL,
        rank TEXT NOT NULL
      );
    \n";
    Tax_id.TaxIdMap.iter
      (fun tax_id name ->
        let rank = Tax_taxonomy.rank_name_of_tax_id tax tax_id in
        Printf.fprintf ch
          "INSERT INTO taxa VALUES (%s, %s, %s);\n"
          (escape (Tax_id.to_string tax_id))
          (escape name)
          (escape rank))
      tax.Tax_taxonomy.tax_name_map;
    close_out ch
end
