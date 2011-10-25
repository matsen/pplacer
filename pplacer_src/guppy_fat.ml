open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit fat_cmd () as super_fat
  inherit placefile_cmd () as super_placefile

  val average = flag "--average"
    (Plain (false, "Average all input placefiles together."))

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_fat#specl
    @ [
      toggle_flag average;
    ]

  method desc =
"makes trees with edges fattened in proportion to the number of reads"
  method usage = "usage: fat [options] placefile[s]"

  method private total_mass_width total_multiplicity =
    match fv width_multiplier with
      | 0. -> fv total_width
      | x -> x *. (float_of_int total_multiplicity)

  method private to_pre_pair tax_info pr =
    let weighting, criterion = self#mass_opts in
    Mass_map.Pre.of_placerun weighting criterion pr,
    try
      match tax_info with
        | None -> None
        | Some (taxt, ti_imap) ->
          let pre =
            Tax_mass.pre
              (Gtree.top_id taxt)
              Placement.classif
              weighting
              criterion
              ti_imap
              pr
          in
          Some (taxt, pre)
    with
      (* if we get a No_classif exception then don't make a tax fat tree *)
      | Placement.No_classif -> None

  method private to_fat_tree final_rt name (phylo_pre, tax_pre) =
    let phylo_mass = Mass_map.By_edge.of_pre phylo_pre in
    [
      Some (name^".ref.fat"),
      self#fat_tree_of_massm final_rt phylo_mass |> self#maybe_numbered
    ]
    @
      (match tax_pre with
        | None -> []
        | Some (taxt, tax_pre) -> begin
          let tax_mass = Mass_map.By_edge.of_pre tax_pre in
          let multiplier_override =
            200. /. (Mass_map.By_edge.total_mass tax_mass)
          in
          [
            Some (name^".tax.fat"),
            self#fat_tree_of_massm ~multiplier_override taxt tax_mass
          ]
        end)

  method private placefile_action prl =
    self#check_placerunl prl;
    let tax_info = match self#get_rpo with
      | None -> None
      | Some rp -> Some (Tax_gtree.of_refpkg_unit rp)
    in
    let pre_pairs = List.map (self#to_pre_pair tax_info) prl in

    if fv average then begin
      let pair = List.fold_left
        (fun (phylo_accum, tax_accum) (phylo_pre, tax_pre) ->
          let phylo_pre' = Mass_map.Pre.normalize_mass phylo_pre in
          List.rev_append phylo_pre' phylo_accum,
          match tax_accum, tax_pre with
            | None, None -> None
            | (Some _ as tax_accum), None -> tax_accum
            | None, (Some _ as tax_accum) -> tax_accum
            | Some (taxt, tax_accum), Some (_, tax_pre) ->
              Some (taxt, List.rev_append tax_pre tax_accum))
        ([], None)
        pre_pairs
      in
      let fname = self#single_file () in
      let trees = self#to_fat_tree
        (snd (self#get_rpo_and_tree (List.hd prl)))
        (safe_chop_suffix fname ".xml")
        pair
      in
      Phyloxml.named_gtrees_to_file
        fname
        trees

    end else begin
      let trees = List.map2
        (fun pr pair ->
          let _, final_rt = self#get_rpo_and_tree pr in
          pr.Placerun.name,
          self#to_fat_tree final_rt pr.Placerun.name pair)
        prl
        pre_pairs
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
            (List.map snd trees |> List.flatten)
        | Unspecified -> ()
    end
end
