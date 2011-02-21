(* pplacer v1.1. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


let () =
  Subcommand.inner_loop
    ~prg_name:"placeutil"
    ~version:"v1.1"
    (Subcommand.cmd_map_of_list
      [
        "round", Placeutil_round.of_argl;
        "demulti", Placeutil_demulti.of_argl;
        "to_json", Placeutil_to_json.of_argl;
        "classify", Placeutil_classify.of_argl;
      ]
    )



