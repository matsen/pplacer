(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * generalized XML nonsense.
 *
 * note that the signature for write_tag is different from that of
 * write_long_tag.
*)

let write_tag write_data_inners tag_name ch data = 
  Printf.fprintf ch "<%s>" tag_name;
  write_data_inners ch data;
  Printf.fprintf ch "</%s>\n" tag_name

let write_long_tag write_inners tag_name ch = 
  write_tag
    (fun _ _ ->
      Printf.fprintf ch "\n";
      write_inners ())
    tag_name
    ch 
    ()

let write_float = write_tag (fun ch x -> Printf.fprintf ch "%g" x)
let write_int = write_tag (fun ch x -> Printf.fprintf ch "%d" x)
let write_string = write_tag (fun ch x -> Printf.fprintf ch "%s" x)


