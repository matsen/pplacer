let of_argl = function
  | [] -> print_endline "converts .place files to .place.json files."
  | argl ->
    (* note-- the command below mutates prefs (so order important) *)
    let fnamel =
      Subcommand.wrap_parse_argv
        argl
        []
        "usage: to_json placefile[s]"
    in
    List.iter begin
      fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (fname ^ ".json") in
        Placerun_io.to_json_file
          (String.concat " " ("placeutil"::argl))
          out_name
          pr
    end fnamel
