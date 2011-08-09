let () = if !Sys.interactive then () else
  Subcommand.inner_loop
    ~prg_name:"rppr"
    ~version:Version.version
    (Subcommand.cmd_map_of_list (Rppr_commands.command_list ()))
