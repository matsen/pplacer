let () =
  if !Sys.interactive then () else Ppatteries.Sparse.error_wrap (fun () ->
    Subcommand.inner_loop
      ~prg_name:"guppy"
      ~version:Version.version
      (Subcommand.cmd_map_of_list (Guppy_commands.command_list ())))

