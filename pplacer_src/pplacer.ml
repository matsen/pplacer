(* "main" *)

open Fam_batteries
open MapsSets

let parse_args () =
  let files  = ref []
  in
  let prefs = Prefs.defaults ()
  and anon_arg arg = files := arg :: !files
  in
  Arg.parse (Prefs.specl prefs) anon_arg Prefs.usage;
  (List.rev !files, prefs)


let () =
  if not !Sys.interactive then begin
    let (files, prefs) = parse_args () in
    if Prefs.version prefs then begin
      print_endline Version.version_revision;
      exit 0;
    end;
    Prefs.check prefs;
    Gsl_error.init ();
    Check.directory (Prefs.out_dir prefs);
    List.iter (Pplacer_run.run_file prefs) files;
  end


