
let () =
  Subcommand.inner_loop
    ~prg_name:"placeutil"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list
      [
        "round", Placeutil_round.of_argl;
        "demulti", Placeutil_demulti.of_argl;
        "to_json", Placeutil_to_json.of_argl;
        "classify", Placeutil_classify.of_argl;
      ]
    )



