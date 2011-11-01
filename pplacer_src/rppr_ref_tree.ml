open Ppatteries
open Guppy_cmdobjs
open Subcommand

let rank_colored_of_refpkg maybe_numbered rp =
  let dt = Refpkg.get_ref_tree rp |> Decor_gtree.of_newick_gtree
  and td = Refpkg.get_taxonomy rp in
  let taxinfo = function
    | [] -> []
    | (hd :: _) as l -> [Decor.Taxinfo (hd, List.map (Tax_taxonomy.get_tax_name td) l |> String.join "|")]
  and st = Gtree.get_stree dt in
  IntMap.fold
    (fun rank taxmap accum ->
      let rankname = Tax_taxonomy.get_rank_name td rank
      and _, cutsetim = Convex.build_sizemim_and_cutsetim (taxmap, st) in
      let cutsetim = IntMap.remove (Stree.top_id st) cutsetim in
      let dt' = IntMap.map
        (Convex.ColorSet.elements |- taxinfo)
        cutsetim
      |> Decor_gtree.add_decor_by_map dt
      in
      (Some rankname, maybe_numbered dt') :: accum)
    (Convex.rank_tax_map_of_refpkg rp)
    []

let trees_of_refpkg maybe_numbered painted colored path =
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
  |> (if colored then
        List.append (rank_colored_of_refpkg maybe_numbered rp)
      else identity)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit numbered_tree_cmd () as super_numbered_tree

  val painted = flag "--painted"
    (Plain (false, "Use a painted tree in place of the taxonomically annotated tree."))
  val colored = flag "--rank-colored"
    (Plain (false, "Include a tree for each rank with taxonomic annotations on every edge."))

  method specl =
    super_output#specl
  @ super_numbered_tree#specl
  @ [
    toggle_flag painted;
    toggle_flag colored;
  ]

  method desc =
"writes a taxonomically annotated reference tree and an induced taxonomic tree"
  method usage = "usage: ref_tree -o my.xml my1.refpkg [my2.refpkg ...]"

  method action = function
    | pathl ->
      let trees = trees_of_refpkg self#maybe_numbered (fv painted) (fv colored) in
      Phyloxml.named_gtrees_to_channel
        self#out_channel
        (List.flatten (List.map trees pathl))

end
