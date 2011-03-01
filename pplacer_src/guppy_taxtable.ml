open Subcommand
open Guppy_cmdobjs

let escape = Base.sqlite_escape

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

      CREATE TABLE IF NOT EXISTS placements (
        name TEXT NOT NULL,
        desired_rank TEXT NOT NULL,
        rank TEXT NOT NULL,
        tax_id TEXT REFERENCES taxa (tax_id) NOT NULL,
        likelihood REAL NOT NULL,
        origin TEXT NOT NULL
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
