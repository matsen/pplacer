open Ppatteries
open Guppy_cmdobjs
open Subcommand

let trees_of_refpkg maybe_numbered painted path =
  let prefix =
    Mokaphy_common.chop_suffix_if_present
      (Refpkg_parse.remove_terminal_slash path) ".refpkg"
  in
  let rp = Refpkg.of_path path in
  let tax_name = Refpkg.get_taxonomy rp |> Tax_taxonomy.get_tax_name in
  let ref_tree =
    if painted then
      Edge_painting.of_refpkg rp
        |> IntMap.map (fun t -> [Decor.Taxinfo (t, tax_name t)])
        |> Decor_gtree.add_decor_by_map
            (Refpkg.get_ref_tree rp |> Decor_gtree.of_newick_gtree)
    else
      Refpkg.get_tax_ref_tree rp
  and (taxt, _) = Tax_gtree.of_refpkg_unit rp in
  [
    Some (prefix^".ref"), maybe_numbered ref_tree;
    Some (prefix^".tax"), taxt;
  ]

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit numbered_tree_cmd () as super_numbered_tree

  val painted = flag "--painted"
    (Plain (false, "Use a painted tree in place of the taxonomically annotated tree."))

  method specl =
    super_output#specl
  @ super_numbered_tree#specl
  @ [toggle_flag painted]

  method desc =
"writes a taxonomically annotated reference tree and an induced taxonomic tree"
  method usage = "usage: ref_tree -o my.xml my1.refpkg [my2.refpkg ...]"

  method action = function
    | pathl ->
      let trees = trees_of_refpkg self#maybe_numbered (fv painted) in
      Phyloxml.pxdata_to_channel self#out_channel
        (Phyloxml.pxdata_of_named_gtrees
           (List.flatten (List.map trees pathl)))

end
