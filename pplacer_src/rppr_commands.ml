let command_list () =
  [
    "rppr", [
      "prep_db", (fun () -> new Rppr_prep_db.cmd ());
      "check", (fun () -> new Rppr_check.cmd ());
      "convexify", (fun () -> new Rppr_convexify.cmd ());
      "ref_tree", (fun () -> new Rppr_ref_tree.cmd ());
      "voronoi", (fun () -> new Rppr_voronoi.cmd ());
      "pdprune", (fun () -> new Rppr_pdprune.cmd ());
      "info", (fun () -> new Rppr_info.cmd ());
      "reroot", (fun () -> new Rppr_reroot.cmd ());
      "infer", (fun () -> new Rppr_infer.cmd ());
      "reclass", (fun () -> new Rppr_reclass.cmd ());
      "prepsim", (fun () -> new Rppr_prepsim.cmd ());
      "vorotree", (fun () -> new Rppr_vorotree.cmd ());
    ];
  ]
