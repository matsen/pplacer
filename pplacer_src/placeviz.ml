
let () =
  Subcommand.inner_loop
    ~prg_name:"placeviz"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list
      [
        "fat", Placeviz_fat.of_argl;
      ]
    )



