open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit fat_cmd () as super_fat

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_fat#specl

  method desc =
"makes trees with edges fattened in proportion to the number of reads"
  method usage = "usage: fat [options] placefile[s]"

  method private total_mass_width total_multiplicity =
    match fv width_multiplier with
      | 0. -> fv total_width
      | x -> x *. (float_of_int total_multiplicity)

  method action fnamel =
    let transform, weighting, criterion = self#mass_opts in
    let trees = List.map
      (fun fname ->
        let pr = Placerun_io.filtered_of_file fname in
        let (tax_rp_opt, final_rt) = self#get_rpo_and_tree pr in
        pr.Placerun.name,
        ([
          Some (pr.Placerun.name^".ref.fat"),
          self#fat_tree_of_massm final_rt
            (Mass_map.By_edge.of_placerun transform weighting criterion pr)
         ]
         @
           (try
              match tax_rp_opt with
                | None -> []
                | Some rp -> begin
                  let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
                  let massm =
                    Mass_map.By_edge.of_pre transform
                      (Tax_mass.pre (Gtree.top_id taxt) Placement.classif
                         weighting criterion ti_imap pr)
                  in
                  let multiplier_override =
                    200. /. (Mass_map.By_edge.total_mass massm)
                  in
                  [
                    Some (pr.Placerun.name^".tax.fat"),
                    self#fat_tree_of_massm ~multiplier_override taxt massm
                  ]
                end
            with
                (* if we get a No_classif exception then don't make a tax fat tree *)
              | Placement.No_classif -> [])))
      fnamel
    in
    match self#out_file_or_dir () with
      | Directory (dir, prefix) ->
        List.iter
          (fun (name, trees) ->
            Phyloxml.named_gtrees_to_file
              (Filename.concat dir (prefix ^ name ^ ".xml"))
              trees)
          trees
      | File fname ->
        Phyloxml.named_gtrees_to_file
          fname
          (Base.map_and_flatten snd trees)
      | Unspecified -> ()
end
