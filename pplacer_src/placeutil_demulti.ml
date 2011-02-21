let demulti_pquery_list pql =
  List.flatten
    (List.map
      (fun pq ->
        List.map
          (fun name -> {pq with Pquery.namel = [name]})
          pq.Pquery.namel)
      pql)

let demulti_placerun out_name pr =
  {pr with
    Placerun.pqueries =
      demulti_pquery_list pr.Placerun.pqueries;
    name = out_name;}

(* UI-related *)

module Prefs = struct
  type prefs =
    {
      out_prefix: string ref;
    }

  let out_prefix p = !(p.out_prefix)

  let defaults () =
    {
      out_prefix = ref "";
    }

  let specl_of_prefs prefs =
[
  "-o", Arg.Set_string prefs.out_prefix,
  "Set the prefix to write to. Required.";
]
end

let of_argl = function
  | [] -> print_endline "splits apart placements with multiplicity, effectively undoing a round procedure."
  | argl ->
    let prefs = Prefs.defaults () in
    (* note-- the command below mutates prefs (so order important) *)
    let fnamel =
      Subcommand.wrap_parse_argv
        argl
        (Prefs.specl_of_prefs prefs)
        "usage: demulti [options] placefile[s]"
    in
    let out_prefix = Prefs.out_prefix prefs in
    if out_prefix = "" then
      invalid_arg "Please specify an output prefix with -o";
    List.iter
      (fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (out_prefix^(pr.Placerun.name)) in
        Placerun_io.to_file
          (String.concat " " ("placeutil"::argl))
          (out_name^".place")
          (demulti_placerun out_name pr))
      fnamel

