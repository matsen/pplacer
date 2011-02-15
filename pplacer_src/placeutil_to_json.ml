(* pplacer v1.1. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

let of_argl = function
  | [] -> print_endline "converts .place files to .place.json files."
  | argl ->
    (* note-- the command below mutates prefs (so order important) *)
    let fnamel =
      Subcommand.wrap_parse_argv
        argl
        []
        "usage: to_json placefile[s]"
    in
    List.iter begin
      fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (fname ^ ".json") in
        Placerun_io.to_json_file
          (String.concat " " ("placeutil"::argl))
          out_name
          pr
    end fnamel
