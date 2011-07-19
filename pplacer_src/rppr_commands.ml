let command_list () =
  [
    "rppr", [
      "taxtable", (fun () -> new Rppr_taxtable.cmd ());
      "check_refpkg", (fun () -> new Rppr_check_refpkg.cmd ());
      "convexify", (fun () -> new Rppr_convexify.cmd ());
      "ref_tree", (fun () -> new Rppr_ref_tree.cmd ());
      "voronoi", (fun () -> new Rppr_voronoi.cmd ());
      "prunetre", (fun () -> new Rppr_prunetre.cmd ());
    ];
  ]
