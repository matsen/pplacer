open Guppy_cmdobjs
open Subcommand

let trees_of_refpkg path =
  let prefix =
    Mokaphy_common.chop_suffix_if_present
      (Refpkg_parse.remove_terminal_slash path) ".refpkg"
  in
  let rp = Refpkg.of_path path in
  let (taxt, _) = Tax_gtree.of_refpkg_unit rp in
  [
    Some (prefix^".ref"), Refpkg.get_tax_ref_tree rp;
    Some (prefix^".tax"), taxt;
  ]

class cmd () =
object (self)
  inherit subcommand () as super
  inherit outfile_cmd () as super_outfile

  method desc =
"writes a taxonomically annotated reference tree and an induced taxonomic tree"
  method usage = "usage: ref_tree -o my.xml my1.refpkg [my2.refpkg ...]"

  method action = function
    | [] -> ()
    | pathl ->
      Phyloxml.pxdata_to_channel self#out_channel
        (Phyloxml.pxdata_of_named_gtrees
           (List.flatten (List.map trees_of_refpkg pathl)))
end
