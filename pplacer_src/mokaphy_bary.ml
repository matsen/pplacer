open MapsSets
open Fam_batteries

module Prefs = struct
  type prefs = 
    {
      out_fname: string ref;
      use_pp: bool ref;
      weighted: bool ref;
      transform: string ref;
    }

  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let weighted          p = !(p.weighted)
  let transform         p = !(p.transform)

  let defaults () =
    {
      out_fname = ref "";
      use_pp = ref false;
      weighted = ref true;
      transform = ref "";
    }

  let specl_of_prefs prefs = 
[
  "-p", Arg.Set prefs.use_pp,
  "Use posterior probability.";
  "--point", Arg.Clear prefs.weighted,
  Mokaphy_prefs.weighted_help;
  "-o", Arg.Set_string prefs.out_fname,
  "Set the filename to write to. Otherwise write to stdout.";
  "--transform", Arg.Set_string prefs.transform,
  Mokaphy_prefs.transform_help;
]
end


let make_bary_tree transform t prel =
  let bary_map = 
    IntMapFuns.of_pairlist_listly
      (ListFuns.mapi
        (fun i pre ->
          let (loc, pos) = Barycenter.of_pre transform t pre in
          (loc,
            (pos, 
            Gtree.Internal_node,
            (fun bl -> 
              new Decor_bark.decor_bark 
                (`Of_bl_name_boot_dlist 
                   (Some bl, None, None, [Decor.dot i]))))))
        prel)
  in
  Gtree.add_subtrees_by_map (Decor_gtree.of_newick_gtree t) bary_map

let bary prefs prl = 
  let t = Cmds_common.list_get_same_tree prl 
  and transform = Mass_map.transform_of_str (Prefs.transform prefs)
  in
  let prel = 
    Cmds_common.prel_of_prl 
      ~is_weighted:(Prefs.weighted prefs)
      ~use_pp:(Prefs.use_pp prefs)
      prl
  in
  if prl <> [] then begin
    let fname = match Prefs.out_fname prefs with
      | "" -> (Cmds_common.cat_names prl)^".bary.xml"
      | s -> s
    in
    Phyloxml.named_tree_to_file
      (Cmds_common.chop_suffix_if_present fname ".xml") (* tree name *)
      (make_bary_tree transform t prel)
      fname
  end

