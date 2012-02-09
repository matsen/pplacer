let command_list () =
  [
    "visualization", [
      "fat", (fun () -> new Guppy_fat.cmd ());
      "heat", (fun () -> new Guppy_heat.cmd ());
      "sing", (fun () -> new Guppy_sing.cmd ());
      "tog", (fun () -> new Guppy_tog.cmd ());
    ];

    "statistical comparison", [
      "bary", (fun () -> new Guppy_bary.cmd ());
      "squash", (fun () -> new Guppy_squash.cmd ());
      "kr_heat", (fun () -> new Guppy_kr_heat.cmd ());
      "kr", (fun () -> new Guppy_kr.cmd ());
      "pca", (fun () -> new Guppy_pca.cmd ());
      "splitify", (fun () -> new Guppy_splitify.cmd ());
      "edpl", (fun () -> new Guppy_edpl.cmd ());
      "pd", (fun () -> new Guppy_pd.cmd ());
      "wpd", (fun () -> new Guppy_wpd.cmd ());
      "entropy", (fun () -> new Guppy_entropy.cmd ());
      "rarefact", (fun () -> new Guppy_rarefact.cmd ());
      "error", (fun () -> new Guppy_error.cmd ());
    ];

    "classification", [
      "classify", (fun () -> new Guppy_classify.cmd ());
    ];

    "utilities", [
      "round", (fun () -> new Guppy_round.cmd ());
      "demulti", (fun () -> new Guppy_demulti.cmd ());
      "to_json", (fun () -> new Guppy_to_json.cmd ());
      "distmat", (fun () -> new Guppy_distmat.cmd ());
      "merge", (fun () -> new Guppy_merge.cmd ());
      "filter", (fun () -> new Guppy_filter.cmd ());
      "info", (fun () -> new Guppy_info.cmd ());
      "redup", (fun () -> new Guppy_redup.cmd ());
      "diplac", (fun () -> new Guppy_diplac.cmd ());
      "to_rdp", (fun () -> new Guppy_to_rdp.cmd ());
      "classify_rdp", (fun () -> new Guppy_classify_rdp.cmd ());
      "mft", (fun () -> new Guppy_mft.cmd ());
      "islands", (fun () -> new Guppy_islands.cmd ());
      "compress", (fun () -> new Guppy_compress.cmd ());
    ];
  ]
