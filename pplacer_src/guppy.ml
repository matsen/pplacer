
let command_list () =
  [
    "visualization", [
      "fat", (fun () -> new Guppy_fat.cmd ());
      "ref_tree", (fun () -> new Guppy_ref_tree.cmd ());
      "sing", (fun () -> new Guppy_sing.cmd ());
    ];

    "statistical comparison", [
      "bary", (fun () -> new Guppy_bary.cmd ());
      "bootviz", (fun () -> new Guppy_bootviz.cmd ());
      "squash", (fun () -> new Guppy_squash.cmd ());
      "heat", (fun () -> new Guppy_heat.cmd ());
      "kr", (fun () -> new Guppy_kr.cmd ());
      "pca", (fun () -> new Guppy_pca.cmd ());
      (* untested so invisible
      "bavgdist", (fun () -> new Guppy_avgdist.bavgdist_cmd ());
      "uavgdist", (fun () -> new Guppy_avgdist.uavgdist_cmd ());
      *)
    ];

    "classification", [
      "classify", (fun () -> new Guppy_classify.cmd ());
    ];

    "utilities", [
      "round", (fun () -> new Guppy_round.cmd ());
      "demulti", (fun () -> new Guppy_demulti.cmd ());
      "to_json", (fun () -> new Guppy_to_json.cmd ());
      "taxtable", (fun () -> new Guppy_taxtable.cmd ());
      "check_refpkg", (fun () -> new Guppy_check_refpkg.cmd ());
      "distmat", (fun () -> new Guppy_distmat.cmd ());
      "merge", (fun () -> new Guppy_merge.cmd ());
    ];
  ]

let () =
  Subcommand.inner_loop
    ~prg_name:"guppy"
    ~version:"v0.1alpha03"
    (Subcommand.cmd_map_of_list (command_list ()))
