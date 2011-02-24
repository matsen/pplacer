open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit out_dir_cmd () as super_out_dir
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd () as super_refpkg
  inherit viz_cmd () as super_viz

  val log_coeff = flag "--log"
    (Plain (0., "Set to a nonzero value to perform a logarithmic transform of the branch width."))

  method specl =
    super_out_prefix#specl
    @ super_out_dir#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_viz#specl
    @ [
      float_flag log_coeff;
    ]

  method desc = "make fat trees"
  method usage = "usage: fat [options] placefile[s]"

  method private total_mass_width total_multiplicity =
    match fv unit_width with
      | 0. -> fv total_width
      | x -> x *. (float_of_int total_multiplicity)

  method action fnamel =
    let transform, weighting, criterion = self#mass_opts
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
