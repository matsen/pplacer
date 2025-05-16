open Subcommand
open Guppy_cmdobjs
open Ppatteries

let prel_of_prl weighting criterion prl =
  List.map (Mass_map.Pre.of_placerun weighting criterion) prl

let make_bary_tree t prel =
  let bary_map =
    IntMap.of_pairlist_listly
      (List.mapi
        (fun i pre ->
          let (loc, pos) = Barycenter.of_pre t pre in
          (loc,
            (pos,
            Gtree.Internal_node,
            (fun bl ->
              new Decor_bark.decor_bark
                (`Of_bl_node_edge_label_decor
                   (Some bl, None, None, [Decor.dot i]))))))
        prel)
  in
  Gtree.add_subtrees_by_map t bary_map

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output
  inherit numbered_tree_cmd () as super_numbered_tree
  inherit refpkg_cmd ~required:false as super_refpkg

  method specl =
    super_refpkg#specl
  @ super_mass#specl
  @ super_output#specl
  @ super_numbered_tree#specl

  method desc =
"draws the barycenter of a placement collection on the reference tree"
  method usage = "usage: bary [options] placefile[s]"

  method private placefile_action prl =
    let t = self#decor_ref_tree_from_placerunl prl
      |> Decor_gtree.add_zero_root_bl
      |> self#maybe_numbered
    and weighting, criterion = self#mass_opts in
    let prel = prel_of_prl weighting criterion prl in
    if prl <> [] then begin
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".heat.xml"))
        ()
      in
      Phyloxml.named_gtree_to_file
        ~fname
        ~tree_name:(safe_chop_suffix fname ".xml") (* tree name *)
        (make_bary_tree t prel)
    end
end
