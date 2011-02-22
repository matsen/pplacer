
let () =
  Subcommand.inner_loop
    ~prg_name:"placeutil"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list
      [
        "round", (fun () -> new Placeutil_round.cmd ());
        "demulti", (fun () -> new Placeutil_demulti.cmd ());
        "to_json", (fun () -> new Placeutil_to_json.cmd ());
        "classify", (fun () -> new Placeutil_classify.cmd ());
      ]
    )



