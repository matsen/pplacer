open Guppy_cmdobjs
open Subcommand

class cmd () =
object
  inherit subcommand () as super
  inherit outfile_cmd () as super_outfile
  inherit refpkg_cmd () as super_refpkg

  method specl = super_outfile#specl @ super_refpkg#specl

  method desc =
    "generates a taxonomically annotated reference tree and an \
    induced taxonomic tree in phyloXML format"
  method usage = "usage: ref_tree -c my.refpkg -o my.xml"

  method action _ =
    let ch = super_outfile#out_channel
    and prefix = Mokaphy_common.chop_suffix_if_present (fv refpkg_path) ".refpkg"
    in
    match Refpkg.refpkgo_of_path (fv refpkg_path) with
    | None -> failwith "please supply a reference package name with -c."
    | Some rp ->
    let (taxt, _) = Tax_gtree.of_refpkg_unit rp in
    Phyloxml.pxdata_to_channel ch
      (Phyloxml.pxdata_of_named_gtrees
        [
          Some (prefix^".ref"), Refpkg.get_tax_ref_tree rp;
          Some (prefix^".tax"), taxt;
        ])
end
