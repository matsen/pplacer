
let command_list () =
  [
    "placeutil", [
      (* mapping place files to place files *)
      "round", (fun () -> new Guppy_round.cmd ());
      "demulti", (fun () -> new Guppy_demulti.cmd ());
      "to_json", (fun () -> new Guppy_to_json.cmd ());

      (* gathering tables from place files *)
      "classify", (fun () -> new Guppy_classify.cmd ());
    ];

    (* making visualizations *)
    "placeviz", [
      "fat", (fun () -> new Guppy_fat.cmd ());
    ];

    (* mokaphy stuff *)
    "mokaphy", [
      "kr", (fun () -> new Guppy_kr.cmd ());
      "pca", (fun () -> new Guppy_pca.cmd ());
      "cluster", (fun () -> new Guppy_cluster.cmd ());
      "heat", (fun () -> new Guppy_heat.cmd ());
      "bavgdist", (fun () -> new Guppy_avgdist.bavgdist_cmd ());
      "uavgdist", (fun () -> new Guppy_avgdist.uavgdist_cmd ());
      "bary", (fun () -> new Guppy_bary.cmd ());
      "bootviz", (fun () -> new Guppy_bootviz.cmd ());
    ];
  ]

let () =
  Subcommand.inner_loop
    ~prg_name:"guppy"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list (command_list ()))
