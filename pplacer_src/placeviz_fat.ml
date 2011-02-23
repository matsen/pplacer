open Subcommand

class cmd () =
object (self)
  inherit subcommand () as super

  val use_pp = flag "-p"
    (Plain (false, "Use posterior probability for the weight."))

  (* mass subcommand *)
  val weighted = flag "--unweighted"
    (Plain (true, "Treat every placement as a point mass concentrated on the highest-weight placement."))
  val transform = flag "--transform"
    (Plain ("", Mokaphy_common.transform_help))

  val out_dir = flag "--out-dir"
    (Plain (".", "Specify the directory to write place files to."))
    (* rp_command *)
  val refpkg_path = flag "-c"
    (Needs_argument ("reference package path", "Reference package path"))
    (* viz_command *)
  val white_bg = flag "--whitebg"
    (Plain (false, "Make colors appropriate for a white background."))
  val min_fat_bl = flag "--min-fat"
    (Formatted (1e-2, "The minimum branch length for fattened edges (to increase their visibility). To turn off set to 0. Default: %g"))
  val total_width = flag "--total-width"
    (Formatted (400., "Set the total number of pixels for all of the mass. Default: %g"))
  val unit_width = flag "--unit-width"
    (Plain (0., "Set the number of pixels for a single placement (will override total-width if set)."))
  val log_coeff = flag "--log"
    (Plain (0., "Set to a nonzero value to perform a logarithmic transform of the branch width."))

  method specl = [
    toggle_flag use_pp;
    toggle_flag weighted;
    string_flag refpkg_path;
    toggle_flag white_bg;
    float_flag min_fat_bl;
    float_flag total_width;
    float_flag unit_width;
    float_flag log_coeff;
    string_flag out_dir;
    string_flag transform;
  ]

  method desc = "make fat trees"
  method usage = "usage: fat [options] placefile[s]"

  method private total_mass_width total_multiplicity =
    match fv unit_width with
      | 0. -> fv total_width
      | x -> x *. (float_of_int total_multiplicity)

      (* rp_command *)
  method private get_rpo_tree pr =
    let alt_tree = Decor_gtree.of_newick_gtree pr.Placerun.ref_tree in
    match Refpkg.refpkgo_of_path (fv refpkg_path) with
      | None -> (None, alt_tree)
      | Some rp ->
        Refpkg.check_tree_identical
          ~epsilon:1e-5
          (pr.Placerun.name^" reference tree")
          (Placerun.get_ref_tree pr)
          rp;
        if Refpkg.tax_equipped rp then (Some rp, Refpkg.get_tax_ref_tree rp)
        else (None, alt_tree)

  method action fnamel =
    let transform = Mass_map.transform_of_str (fv transform)
    and weighting = if (fv weighted) then Mass_map.Weighted else Mass_map.Unweighted
    and criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio
    and min_bl = match (fv min_fat_bl) with 0. -> None | x -> Some x
    in
    List.iter
      (fun fname ->
        let pr = Placerun_io.filtered_of_file fname in
        let (tax_rp_opt, final_rt) = self#get_rpo_tree pr
        and mass_width = self#total_mass_width (Placerun.total_multiplicity pr)
        in
        Phyloxml.named_gtrees_to_file
          (pr.Placerun.name^".xml")
          ([
            Some (pr.Placerun.name^".ref.fat"),
            Placeviz_core.fat_tree ?min_bl mass_width (fv log_coeff) final_rt
              (Mass_map.By_edge.of_placerun transform weighting criterion pr)
           ]
           @
           (try
              match tax_rp_opt with
                | None -> []
                | Some rp -> begin
                  let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
                  [
                    Some (pr.Placerun.name^".tax.fat"),
                    Placeviz_core.fat_tree (mass_width /. 2.) (fv log_coeff) taxt
                      (Mass_map.By_edge.of_pre transform
                         (Tax_mass.pre (Gtree.top_id taxt) Placement.contain_classif
                            weighting criterion ti_imap pr))
                  ]
                end
              with
        (* if we get a No_classif exception then don't make a tax fat tree *)
                | Placement.No_classif -> [])))
      fnamel
end
