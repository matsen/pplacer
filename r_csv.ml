(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Parse R CSV files into string opt array arrays
 * we dequote the quoted
*)

(* we just remove the outermost set of quotes, even if there are quotes on the
 * inside *)
let quote_rex = Str.regexp "[ ]*\"\\(.*\\)\"[ ]*"
let comma_rex = Str.regexp ","

let dequote s = 
  if Str.string_match quote_rex s 0 then
    Str.matched_group 1 s
  else
    s

let entry_of_str s = 
  if s = "NA" || s = "\"\"" then None
  else Some (dequote s)

(* returns a list of arrays *)
let list_list_of_file fname = 
  List.map
    (fun line ->
      List.map entry_of_str (Str.split comma_rex line))
    (File_parsing.string_list_of_file fname)
    
(* if a list list is rectangular *)
let list_list_is_rectangular = function
  | x::l -> begin
      try
        let x_l = List.length x in
        List.iter
          (fun y -> if x_l <> List.length y then raise Exit)
          l;
        true
      with
      | Exit -> false
    end
  | [] -> true
  
