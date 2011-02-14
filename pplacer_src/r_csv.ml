(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * A little wrapper for the Csv module such that empty strings and NA become
 * None.
*)

(* *** READING *** *)

let entry_of_str = function
  | "" -> None
  | "NA" -> None
  | s -> Some s

(* returns a list of arrays *)
let list_list_of_file fname =
  List.map (List.map entry_of_str) (Csv.load fname)

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


(* *** WRITING *** *)

let quote s = "\""^s^"\""
let strl_to_str strl = String.concat "," strl
let write_strl ch strl = Printf.fprintf ch "%s\n" (strl_to_str strl)
