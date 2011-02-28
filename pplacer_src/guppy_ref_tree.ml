open Guppy_cmdobjs
open Subcommand

let trees_of_refpkg path =
  let prefix =
    Mokaphy_common.chop_suffix_if_present
      (Refpkg_parse.remove_terminal_slash path) ".refpkg"
  in
  match Refpkg.refpkgo_of_path path with
  | None -> []
  | Some rp ->
      let (taxt, _) = Tax_gtree.of_refpkg_unit rp in
      [
        Some (prefix^".ref"), Refpkg.get_tax_ref_tree rp;
        Some (prefix^".tax"), taxt;
      ]

class cmd () =
object
  inherit subcommand () as super
  inherit outfile_cmd () as super_outfile
  inherit refpkg_cmd () as super_refpkg

  method specl = super_outfile#specl @ super_refpkg#specl

  method desc =
"writes a taxonomically annotated reference tree and an induced taxonomic tree"
  method usage = "usage: ref_tree -o my.xml my1.refpkg [my2.refpkg ...]"

  method action pathl =
    Phyloxml.pxdata_to_channel super_outfile#out_channel
      (Phyloxml.pxdata_of_named_gtrees
        (List.flatten (List.map trees_of_refpkg pathl)))
end
