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

class cmd () =
object
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile

  val outfile = flag "-o"
    (Plain ("", "Output file. Default is derived from the input filenames."))
  val default_exclude = flag "-v"
    (Plain (false, "Exclude everything by default."))
  val regexp_inclusions = flag "-Ir"
    (Plain ([], "Include placements whose name matches the given regexp. May be passed multiple times."))
  val regexp_exclusions = flag "-Er"
    (Plain ([], "Exclude placements whose name matches the given regexp. May be passed multiple times."))

  method specl = [
    string_flag outfile;
    toggle_flag default_exclude;
    string_list_flag regexp_inclusions;
    string_list_flag regexp_exclusions;
  ]

  method desc = "filters one or more placefiles by placement name"
  method usage = "usage: filter [options] ex1.place [ex2.place [...]]"

  method private placefile_action = function
    | [] -> ()
    | prl ->
      let fname = match fv outfile with
        | "" -> (Mokaphy_common.cat_names prl) ^ ".json"
        | s -> s
      in
      let r_inclusions = List.map Str.regexp (List.rev (fv regexp_inclusions))
      and r_exclusions = List.map Str.regexp (List.rev (fv regexp_exclusions))
      and default_exclude = fv default_exclude in
      let r_included = any_match r_inclusions
      and r_excluded = any_match r_exclusions in
      let fold_pq pqs pq =
        let namel = List.filter
          (fun name ->
            if default_exclude then
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
