(* Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let option_rex = Str.regexp "-.*"

let print_avail_cmds prg_name cmd_map = 
  print_endline "Here is a list of commands available using this interface:";
  StringMap.iter (fun k v -> Printf.printf "\t%s\t" k; v []) cmd_map;
  Printf.printf 
    "To get more help about a given command, type %s COMMAND --help\n"
    prg_name;
  ()


let process_cmd prg_name cmd_map argl = 
  let print_need_cmd_error () = 
    Printf.printf 
      "please specify a %s command, e.g. %s COMMAND [...]"
      prg_name prg_name;
    print_avail_cmds prg_name cmd_map;
    exit 1
  in
  match argl with
    | s::_ -> 
      if StringMap.mem s cmd_map then
        (StringMap.find s cmd_map) argl
      else if Str.string_match option_rex s 0 then 
        print_need_cmd_error ()
      else begin
        print_endline ("Unknown "^prg_name^" command: "^s);
        print_avail_cmds prg_name cmd_map;
        exit 1
      end
    | [] -> print_need_cmd_error ()



(* externally facing *)

(* this takes an argument list, a specification list, and a usage string, does
 * the relevant parsing, and then spits out a list of anonymous arguments (those
 * not associated with command line flags *)
let wrap_parse_argv argl specl usage = 
  let anonymous = ref [] in
  try
    Arg.parse_argv 
      ~current:(ref 0) (* start from beginning *)
      (Array.of_list argl) 
      specl
      (fun s -> anonymous := s::!anonymous)
      usage;
    (* we assume that some anonymous argument are needed *)
    if !anonymous = [] then begin
      print_endline usage;
      exit 0;
    end;
    List.rev !anonymous
  with
  | Arg.Bad s -> print_string s; exit 1
  | Arg.Help s -> print_string s; []


(* makes a specification with a default value *)
let spec_with_default symbol setfun p help = 
  (symbol, setfun p, Printf.sprintf help !p)


(* given a (string, f) list, make a map of it *)
let cmd_map_of_list l = 
  List.fold_right (fun (k,v) -> StringMap.add k v) l StringMap.empty


(* intended to be the inner loop of a function *)
let inner_loop ~prg_name ~version cmd_map = 
  Arg.parse
    [
      "-v", Arg.Unit (fun () -> Printf.printf "placeutil %s\n" version),
      "Print version and exit";
      "--cmds", Arg.Unit (fun () -> print_avail_cmds prg_name cmd_map),
      "Print a list of the available commands.";
    ]
    (fun _ -> (* anonymous args. tl to remove command name. *)
      process_cmd prg_name cmd_map (List.tl (Array.to_list Sys.argv));
      exit 0) (* need to exit to avoid processing the other anon args as cmds *)
    (Printf.sprintf 
      "Type %s --cmds to see the list of available commands."
      prg_name)
