(* pplacer v1.1. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

module Prefs = struct
  type prefs = 
    {
      out_fname: string ref;
    }

  let defaults () =
    {
      out_fname = ref "";
    }

  let specl_of_prefs prefs = 
[
  "-o", Arg.Set_string prefs.out_fname,
  "Set the filename to write to. Otherwise write to stdout.";
]
end

let cluster prefs prl = 
  let _ = prefs in 
  Printf.printf "%d\n" (List.length prl)

let of_argl = function
  | [] -> print_endline "clusters the placements by rounding"
  | argl -> 
    let prefs = Prefs.defaults () in
    cluster
      prefs 
      (Subcommand.wrap_parse_argv
        argl
        (Prefs.specl_of_prefs prefs)
        "usage: cluster [options] placefile[s]")

