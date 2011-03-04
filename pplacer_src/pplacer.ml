(* "main" *)

open Fam_batteries
open MapsSets

let parse_args () =
  let files  = ref []
  and prefs = Prefs.defaults ()
  in
  let usage =
    "pplacer "^Version.version_revision^"\npplacer [options] [alignment]\n"
  and anon_arg arg =
    files := arg :: !files
  in
  Arg.parse (Prefs.args prefs) anon_arg usage;
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
    if Prefs.verb_level prefs >= 1 then begin
      Common_base.print_elapsed_time ();
      Common_base.print_n_compactions ();
      print_endline "";
    end
  end


