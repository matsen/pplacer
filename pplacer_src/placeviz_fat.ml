open Placeviz_common

module Prefs = struct
  type prefs =
    {
      use_pp: bool ref;
      weighted: bool ref;
      refpkg_path: string ref;
      white_bg: bool ref;
      min_fat_bl: float ref;
      unit_width: float ref;
      log_coeff: float ref;
      total_width: float ref;
      out_dir: string ref;
      transform: string ref;
    }


  let defaults () =
    {
      use_pp = ref false;
      weighted = ref true;
      refpkg_path = ref "";
      white_bg = ref false;
      min_fat_bl = ref 1e-2;
      log_coeff = ref 0.;
      unit_width = ref 0.;
      total_width = ref 400.;
      out_dir = ref ".";
      transform = ref "";
    }

  let specl_of_prefs prefs =
    let _ = prefs in
[
  "-c", Arg.Set_string prefs.refpkg_path,
  "Reference package path";
  (* nothing until we get oo radness *)
]
end

let weighting_of_prefs prefs =
  if !(prefs.Prefs.weighted) then Mass_map.Weighted
  else Mass_map.Unweighted

let criterion_of_prefs prefs =
  if !(prefs.Prefs.use_pp) then Placement.post_prob
  else Placement.ml_ratio

let total_mass_width prefs total_multiplicity =
  if !(prefs.Prefs.unit_width) <> 0. then (* unit width specified *)
    !(prefs.Prefs.unit_width) *. (float_of_int total_multiplicity)
  else (* split up the mass according to the number of queries *)
    !(prefs.Prefs.total_width)

let get_rpo_tree prefs pr =
  let alt_tree = Decor_gtree.of_newick_gtree (Placerun.get_ref_tree pr) in
  match Refpkg.refpkgo_of_path !(prefs.Prefs.refpkg_path) with
  | None -> (None, alt_tree)
  | Some rp ->
      if Refpkg.tax_equipped rp then (Some rp, Refpkg.get_tax_ref_tree rp)
      else (None, alt_tree)

let of_argl = function
  | [] -> print_endline "make fat trees"
  | argl ->
    let prefs = Prefs.defaults () in
    (* note-- the command below mutates prefs (so order important) *)
    let fnamel =
      Subcommand.wrap_parse_argv
        argl
        (Prefs.specl_of_prefs prefs)
        "usage: fat [options] placefile[s]"
    in
    if fnamel = [] then exit 0;
    let transform = Mass_map.transform_of_str !(prefs.Prefs.transform)
    and weighting = weighting_of_prefs prefs
    and criterion = criterion_of_prefs prefs
    and min_bl = match !(prefs.Prefs.min_fat_bl) with 0. -> None | x -> Some x
    in
    List.iter
      (fun fname ->
      let pr = Placerun_io.filtered_of_file fname in
      let (tax_rp_opt, final_rt) = get_rpo_tree prefs pr
      and mass_width = total_mass_width prefs (Placerun.total_multiplicity pr)
      in
      Phyloxml.named_gtrees_to_file
        (pr.Placerun.name^".xml")
        ([
          Some (pr.Placerun.name^".ref.fat"),
          Placeviz_core.fat_tree ?min_bl mass_width !(prefs.Prefs.log_coeff) final_rt
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
              Placeviz_core.fat_tree (mass_width /. 2.) !(prefs.Prefs.log_coeff) taxt
                (Mass_map.By_edge.of_pre transform
                  (Tax_mass.pre (Gtree.top_id taxt) Placement.contain_classif
                    weighting criterion ti_imap pr))
            ]
            end
        with
      (* if we get a No_classif exception then don't make a tax fat tree *)
        | Placement.No_classif -> [])))
      fnamel

      (*

      | Placement.No_PP ->
          failwith "Posterior probability use requested, but some or all placements were calculated without PP."
          *)
