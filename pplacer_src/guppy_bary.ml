open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let prel_of_prl weighting criterion prl =
  List.map (Mass_map.Pre.of_placerun weighting criterion) prl

let make_bary_tree transform t prel =
  let bary_map =
    IntMap.of_pairlist_listly
      (ListFuns.mapi
        (fun i pre ->
          let (loc, pos) = Barycenter.of_pre transform t pre in
          (loc,
            (pos,
            Gtree.Internal_node,
            (fun bl ->
              new Decor_bark.decor_bark
                (`Of_bl_name_boot_decor
                   (Some bl, None, None, [Decor.dot i]))))))
        prel)
  in
  Gtree.add_subtrees_by_map (Decor_gtree.of_newick_gtree t) bary_map

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl = super_mass#specl @ super_output#specl

  method desc =
"draws the barycenter of a placement collection on the reference tree"
  method usage = "usage: bary [options] placefile[s]"

  method private placefile_action prl =
    let t = Mokaphy_common.list_get_same_tree prl
    and transform, weighting, criterion = self#mass_opts
    in
    let prel = prel_of_prl weighting criterion prl
    in
    if prl <> [] then begin
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".heat.xml"))
        ()
      in
      Phyloxml.named_gtree_to_file
        fname
        (Mokaphy_common.chop_suffix_if_present fname ".xml") (* tree name *)
        (make_bary_tree transform t prel)
    end
end
