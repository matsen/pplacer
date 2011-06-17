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

  method private to_mass_pair tax_info pr =
    let transform, weighting, criterion = self#mass_opts in
    Mass_map.By_edge.of_placerun transform weighting criterion pr,
    try
      match tax_info with
        | None -> None
        | Some (taxt, ti_imap) ->
          let massm =
            Mass_map.By_edge.of_pre transform
              (Tax_mass.pre (Gtree.top_id taxt) Placement.classif
                 weighting criterion ti_imap pr)
          in
          Some (taxt, massm)
    with
      (* if we get a No_classif exception then don't make a tax fat tree *)
      | Placement.No_classif -> None

  method private to_fat_tree final_rt pr (phylo_mass, tax_mass) =
    [
      Some (pr.Placerun.name^".ref.fat"),
      self#fat_tree_of_massm final_rt phylo_mass
    ]
    @
      (match tax_mass with
        | None -> []
        | Some (taxt, massm) -> begin
          let multiplier_override =
            200. /. (Mass_map.By_edge.total_mass massm)
          in
          [
            Some (pr.Placerun.name^".tax.fat"),
            self#fat_tree_of_massm ~multiplier_override taxt massm
          ]
        end)

  method action fnamel =
    let prl = List.map Placerun_io.filtered_of_file fnamel in
    self#check_placerunl prl;
    let tax_info = match self#get_rpo with
      | None -> None
      | Some rp -> Some (Tax_gtree.of_refpkg_unit rp)
    in
    let mass_maps = List.map (self#to_mass_pair tax_info) prl in
    let trees = List.map2
      (fun pr pair ->
        let _, final_rt = self#get_rpo_and_tree pr in
        pr.Placerun.name,
        self#to_fat_tree final_rt pr pair)
      prl
      mass_maps
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
