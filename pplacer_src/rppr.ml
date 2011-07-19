let () =
  Subcommand.inner_loop
    ~prg_name:"rppr"
    ~version:Version.version_revision
    (Subcommand.cmd_map_of_list (Rppr_commands.command_list ()))
