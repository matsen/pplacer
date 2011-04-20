open Subcommand
open Guppy_cmdobjs

let any_match rl s =
  List.exists
    (fun r ->
      try
        let _ = Str.search_forward r s 0 in true
      with
        | Not_found -> false)
    rl

module CF = Guppy_classify
let classify how criterion n_ranks td pq =
  let outl = ref [] in
  let m = ref
    (List.fold_right
       (fun p ->
         CF.TIAMR.add_by
           (how p)
           (criterion p))
       (Pquery.place_list pq)
       (CF.TIAMR.M.empty))
  in
  for desired_rank=(n_ranks-1) downto 0 do
    m := CF.keymap_add_by (CF.classify_at_rank td desired_rank) !m;
    outl := (Tax_id.TaxIdMapFuns.to_pairs !m) :: !outl
  done;
  List.flatten !outl

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile

  val outfile = flag "-o"
    (Plain ("", "Output file. Default is derived from the input filenames."))
  val regexp_default_exclude = flag "-Vr"
    (Plain (false, "Exclude every placement name by default."))
  val regexp_inclusions = flag "-Ir"
    (Plain ([], "Include placements whose name matches the given regexp. May be passed multiple times."))
  val regexp_exclusions = flag "-Er"
    (Plain ([], "Exclude placements whose name matches the given regexp. May be passed multiple times."))
  val tax_cutoff = flag "--cutoff"
    (Formatted (0.9, "Use this cutoff for determining how likely a match is for a tax_id. Default: %g"))
  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria."))
  val tax_default_exclude = flag "-Vx"
    (Plain (false, "Exclude every tax_id by default."))
  val tax_inclusions = flag "-Ix"
    (Plain ([], "Include placements which are likely matches for the given tax_id. May be passed multiple times."))
  val tax_exclusions = flag "-Ex"
    (Plain ([], "Exclude placements which are likely matches for the given tax_id. May be passed multiple times."))

  method specl = [
    string_flag outfile;
    toggle_flag regexp_default_exclude;
    string_list_flag regexp_inclusions;
    string_list_flag regexp_exclusions;
  ] @ super_refpkg#specl @ [
    float_flag tax_cutoff;
    toggle_flag use_pp;
    toggle_flag tax_default_exclude;
    string_list_flag tax_inclusions;
    string_list_flag tax_exclusions;
  ]

  method desc = "filters one or more placefiles by placement name"
  method usage = "usage: filter [options] placefile[s]"

  method private placefile_action = function
    | [] -> ()
    | prl ->
      let fname = match fv outfile with
        | "" -> (Mokaphy_common.cat_names prl) ^ ".json"
        | s -> s
      in
      let r_inclusions = List.map Str.regexp (List.rev (fv regexp_inclusions))
      and r_exclusions = List.map Str.regexp (List.rev (fv regexp_exclusions))
      and r_default_exclude = fv regexp_default_exclude
      and t_inclusions = List.rev (fv tax_inclusions)
      and t_exclusions = List.rev (fv tax_exclusions)
      and t_default_exclude = fv tax_default_exclude in
      let tax_pred =
        if t_inclusions = [] && t_exclusions = [] then
          (fun _ -> true)
        else
          let criterion =
            if fv use_pp then Placement.post_prob
            else Placement.ml_ratio
          and td = Refpkg.get_taxonomy (self#get_rp) in
          let n_ranks = Tax_taxonomy.get_n_ranks td
          and cutoff = fv tax_cutoff in
          let tax_match candidates classified =
            List.exists
              (fun candidate ->
                List.exists
                  (fun (tid, prob) ->
                    Tax_id.to_string tid = candidate && prob > cutoff)
                  classified)
              candidates
          in
          let t_included = tax_match t_inclusions
          and t_excluded = tax_match t_exclusions
          and classify = classify
            Placement.classif
            criterion
            n_ranks
            td
          in (fun pq ->
            let cfied = classify pq in
            if t_default_exclude then
              (t_included cfied) && (not (t_excluded cfied))
            else
              (not (t_excluded cfied)) || (t_included cfied))
      in
      let r_included = any_match r_inclusions
      and r_excluded = any_match r_exclusions in
      let fold_pq pqs pq =
        if not (tax_pred pq) then
          pqs
        else
          let namel = List.filter
            (fun name ->
              if r_default_exclude then
                (r_included name) && (not (r_excluded name))
              else
                (not (r_excluded name)) || (r_included name))
            (Pquery.namel pq)
          in
          if namel = [] then
            pqs
          else
            {pq with Pquery.namel = namel} :: pqs
      in
      let filtered = List.map
        (fun pr ->
          Placerun.set_pqueries
            pr
            (List.fold_left fold_pq [] (Placerun.get_pqueries pr)))
        prl
      in
      let combined = List.fold_left
        (Placerun.combine "")
        (List.hd filtered)
        (List.tl filtered)
      in
      Placerun_io.to_json_file "guppy filter" fname combined
end
