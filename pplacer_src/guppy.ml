
let command_list () =
  [
    "visualization", [
      "fat", (fun () -> new Guppy_fat.cmd ());
      "ref_tree", (fun () -> new Guppy_ref_tree.cmd ());
    ];

    "statistical comparison", [
      "bary", (fun () -> new Guppy_bary.cmd ());
      "bavgdist", (fun () -> new Guppy_avgdist.bavgdist_cmd ());
      "bootviz", (fun () -> new Guppy_bootviz.cmd ());
      "cluster", (fun () -> new Guppy_cluster.cmd ());
      "heat", (fun () -> new Guppy_heat.cmd ());
      "kr", (fun () -> new Guppy_kr.cmd ());
      "pca", (fun () -> new Guppy_pca.cmd ());
      "uavgdist", (fun () -> new Guppy_avgdist.uavgdist_cmd ());
    ];

    "classification", [
      "classify", (fun () -> new Guppy_classify.cmd ());
    ];

    "utilities", [
      "round", (fun () -> new Guppy_round.cmd ());
      "demulti", (fun () -> new Guppy_demulti.cmd ());
      "to_json", (fun () -> new Guppy_to_json.cmd ());
    ];
  ]

let () =
  Subcommand.inner_loop
    ~prg_name:"guppy"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list (command_list ()))
