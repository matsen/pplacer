(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets


let refpkg_str = "CONTENTS.txt"


(* *** parsing *** *)

(* (nonempty) then space then = then (nothing or something ending in something
 * nonempty) then space *)
let equality_rex = 
  Str.regexp "\\([^ \t]+\\)[ \t]+=[ \t]*\\(\\|.*[^ \t]\\)[ \t]*$"

let eqpair_of_str s = 
  if Str.string_match equality_rex s 0 then
    (Str.matched_group 1 s, Str.matched_group 2 s)
  else
    failwith ("equality_pair_of_str: malformed string: "^s)

let eqmap_of_strl sl = 
  List.fold_right
    (fun (k,v) -> StringMap.add k v)
    (List.map eqpair_of_str sl)
    StringMap.empty

 (* parsing sectioned files *)

let sechead_rex = Str.regexp "^[ \t]*\\[\\([^]]*\\)\\][ \t]*"

let extract_secheado s = 
  if not (Str.string_match sechead_rex s 0) then None
  else Some (Str.matched_group 1 s)

let secmap_of_strl strl = 
  let rec aux m curr_sec = function
    | x::l -> 
        (match extract_secheado x with
        | Some sec -> aux m sec l
        | None -> aux (StringMapFuns.add_listly curr_sec x m) curr_sec l)
    | [] -> m
  in
  match File_parsing.filter_empty_lines strl with
    | x::l ->
        (match extract_secheado x with
        | Some sec -> StringMap.map List.rev (aux StringMap.empty sec l)
        | None -> invalid_arg "You must start config file with a section header!")
    | [] -> StringMap.empty

let remove_terminal_slash s = 
  let len = String.length s in
  if s.[len - 1] <> '/' then s
  else String.sub s 0 (len-1)

let strmap_of_path path = 
  if not (Sys.is_directory path) then
    failwith ("Purported refpkg "^path^" is not a directory");
  let noslash = remove_terminal_slash path in
  let dirize fname = noslash^"/"^fname in
  let secmap = 
    secmap_of_strl 
      (try File_parsing.string_list_of_file (dirize refpkg_str) with
      | Sys_error _ -> invalid_arg (Printf.sprintf "can't find %s in %s" refpkg_str path))
  in
  let get_sec s = 
    try StringMap.find s secmap with
    | Not_found -> invalid_arg ("missing section "^s^" in refpkg "^path) 
  in
  StringMap.add "name" (Filename.basename noslash) 
                (StringMap.map dirize (eqmap_of_strl (get_sec "files")))
