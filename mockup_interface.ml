(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

let version = "v1.0"

(* *** regexps and utils *** *)
let split_on_space s = Str.split (Str.regexp "[ \t]+") s
let place_file_rex = Str.regexp ".*\\.place"
let option_rex = Str.regexp "-.*"
let print_need_cmd_error () = 
  print_endline "please specify a mokaphy command, e.g. mokaphy heat [...]";
  exit 1

(* *** accessing placefiles *** *)
(* our strategy is to load the placefiles in to memory when we need them, but if
  * they are already in memory, then we use them *)
let placerun_map = ref StringMap.empty
let placerun_by_name fname = 
  if not (Str.string_match place_file_rex fname 0) then
    failwith ("Place files must end with .place suffix, unlike: "^fname);
  if StringMap.mem fname !placerun_map then
    StringMap.find fname !placerun_map
  else begin
    let pq = Placerun_io.of_file fname in
    placerun_map := StringMap.add fname pq !placerun_map;
    pq
  end

(* *** wrapped versions of programs *** *)
(* parse_argv wrapper to factor the drudgery *)
let wrap_parse_argv argl specl usage = 
  let files = ref [] in
  let anon_arg s = files := s::!files in
  try
    Arg.parse_argv 
      ~current:(ref 0) (* start from beginning *)
      (Array.of_list argl) 
      specl
      anon_arg
      usage;
    if !files = [] then begin
      print_string "No .place files supplied so nothing to do. ";
      print_endline usage;
    end;
    List.map placerun_by_name (List.rev !files)
  with
  | Arg.Bad s -> print_string s; exit 1
  | Arg.Help s -> print_string s; []

(* here are the commands, wrapped up to simply take an argument list. they must
 * also print out a documentation line when given an empty list argument. *)
let bary_of_argl = function
  | [] -> print_endline "draws the barycenter of a placement collection on the reference tree"
  | argl -> 
    let prefs = Mockup_prefs.Bary.defaults () in
    Cmds.bary 
      prefs 
      (wrap_parse_argv
        argl
        (Mockup_prefs.Bary.specl_of_prefs prefs)
        "usage: bary [options] placefile[s]")

let heat_of_argl = function
  | [] -> print_endline "makes a heat tree given two placefiles"
  | argl -> 
    let prefs = Mockup_prefs.Heat.defaults () in
    Cmds.heat 
      prefs 
      (wrap_parse_argv
        argl
        (Mockup_prefs.Heat.specl_of_prefs prefs)
        "usage: heat [options] ex1.place ex2.place")

let cmd_map = 
  List.fold_right 
    (fun (k,v) -> StringMap.add k v)
    [
      "bary", bary_of_argl;
    ]
    StringMap.empty

let print_avail_cmds () = 
  print_endline "Here is a list of commands available using this interface:";
  StringMap.iter (fun k v -> Printf.printf "\t%s\t" k; v []) cmd_map;
  print_endline "To get more help about a given command, type mokaphy [program_name] --help";
  ()

(* *** inner loop *** *)
let process_cmd = function
  | s::_ as argl -> 
      if StringMap.mem s cmd_map then
        (StringMap.find s cmd_map) argl
      else if Str.string_match option_rex s 0
           || Str.string_match place_file_rex s 0 then 
        print_need_cmd_error ()
      else begin
        print_endline ("Unknown mokaphy command: "^s);
        print_avail_cmds ();
        exit 1
      end
  | [] -> print_need_cmd_error ()

let process_batch_file fname =
  List.iter 
    (fun s -> process_cmd (split_on_space s))
    (File_parsing.filter_comments 
      (File_parsing.string_list_of_file fname))

let () = begin
    Arg.parse
      [
        "-B", Arg.String process_batch_file,
        "Execute commands from indicated batch file";
        "-v", Arg.Unit (fun () -> Printf.printf "mokaphy %s\n" version),
        "Print version and exit";
        "--cmds", Arg.Unit print_avail_cmds,
        "Print a list of the available commands.";
      ]
      (fun _ -> (* anonymous args. tl to remove "mokaphy" or symlink name *)
        process_cmd (List.tl (Array.to_list Sys.argv));
        exit 0) (* need to exit to avoid processing the other anon args as cmds *)
      "mokaphy can be used as mokaphy [command name] [args] \
      or -B [batch file] to run a batch analysis; \
      Type mokaphy --cmds to see the list of available commands." 
end
