(* "main" *)

open Ppatteries

let parse_args () =
  let files  = ref []
  in
  let prefs = Prefs.defaults ()
  and anon_arg arg = files := arg :: !files
  in
  Arg.parse (Prefs.specl prefs) anon_arg Prefs.usage;
  (List.rev !files, prefs)


let () =
  if not !Sys.interactive then begin exn_wrap (fun () ->
    let (files, prefs) = parse_args () in
    if Prefs.version prefs then begin
      print_endline Version.version;
      exit 0;
    end;
    Prefs.check prefs;
    Check.directory (Prefs.out_dir prefs);
    if List.length files = 0 then
      print_endline
        "Warning: pplacer couldn't find any sequences to place. Please supply \
        an alignment with sequences to place as an argument at the end of \
        the command line."
    else if List.length files > 1 && Prefs.out_file prefs <> "" then
      failwith "`-o` may not be specified with multiple alignments.";
    List.iter (Pplacer_run.run_file prefs) files)
  end
