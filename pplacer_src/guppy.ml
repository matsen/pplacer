
let () =
  Subcommand.inner_loop
    ~prg_name:"guppy"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list
      [
        (* mapping place files to place files *)
        "round", (fun () -> new Placeutil_round.cmd ());
        "demulti", (fun () -> new Placeutil_demulti.cmd ());
        "to_json", (fun () -> new Placeutil_to_json.cmd ());

        (* gathering tables from place files *)
        "classify", (fun () -> new Placeutil_classify.cmd ());

        (* making visualizations *)
        "fat", (fun () -> new Placeviz_fat.cmd ());

        (* mokaphy stuff *)
        "kr", (fun () -> new Mokaphy_kr.cmd ());
        "pca", (fun () -> new Mokaphy_pca.cmd ());
        "cluster", (fun () -> new Mokaphy_cluster.cmd ());
        "heat", (fun () -> new Mokaphy_heat.cmd ());
        "bavgdist", (fun () -> new Mokaphy_avgdist.bavgdist_cmd ());
        "uavgdist", (fun () -> new Mokaphy_avgdist.uavgdist_cmd ());
        "bary", (fun () -> new Mokaphy_bary.cmd ());
        "bootviz", (fun () -> new Mokaphy_bootviz.cmd ());
      ]
    )
