(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

(* regexps and utils *)
let split_space s = Str.split (Str.regexp "[ \t]+") s
let place_file_rex = Str.regexp ".*\\.place"
let option_rex = Str.regexp "-.*"
let demokaphy = function | "mokaphy"::l -> l | l -> l
let print_need_cmd_error () = 
  print_endline "please specify a mokaphy command, e.g. mokaphy heat [...]";
  exit 1

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

let bary prefs datal = 
  Printf.printf "%s\n" (Mockup_prefs.Bary.out_fname prefs);
  List.iter print_endline (List.map Placerun.get_name datal)

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
    List.map placerun_by_name (List.rev !files)
  with
  | Arg.Bad s -> print_string s; exit 1
  | Arg.Help s -> print_string s; []

let process_cmd = function
  | "bary"::_ as argl -> begin
      let prefs = Mockup_prefs.Bary.defaults () in
      bary 
        prefs 
        (wrap_parse_argv
          argl
          (Mockup_prefs.Bary.specl_of_prefs prefs)
          "usage: bary [options] placefiles")
      end
  | s::_ -> 
      if Str.string_match option_rex s 0
      || Str.string_match place_file_rex s 0 then 
        print_need_cmd_error ()
      else
        failwith ("unknown command: "^s)
  | [] -> print_need_cmd_error ()

let process_cmd_str s = 
  process_cmd (demokaphy (split_space s))

let () = begin
  List.iter 
    process_cmd_str 
    [
      "mokaphy bary -o test top_tests/test1.place top_tests/test2.place";
    ];

end
