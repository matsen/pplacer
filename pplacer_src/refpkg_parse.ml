(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets


let refpkg_str = "CONTENTS.json"

let sstringMap_of_Sjobj o = 
  Hashtbl.fold (fun k v -> StringMap.add k (Simple_json.get_string v)) 
               (Simple_json.get_hashtbl o) 
               StringMap.empty

(* *** parsing *** *)

let remove_terminal_slash s = 
  let len = String.length s in
  if s.[len - 1] <> '/' then s
  else String.sub s 0 (len-1)

let strmap_of_path path = 
  if not (Sys.is_directory path) then
    failwith ("Purported refpkg "^path^" is not a directory");
  let noslash = remove_terminal_slash path in
  let dirize fname = noslash^"/"^fname in
  let contents = Simple_json.of_file (dirize refpkg_str)
  in
  StringMap.add
    "name" 
    (Base.safe_chop_suffix (Filename.basename noslash) ".refpkg")
    (StringMap.map dirize 
      (sstringMap_of_Sjobj (Simple_json.find contents "files")))
