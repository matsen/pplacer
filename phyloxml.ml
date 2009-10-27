(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * aah... functional programming.
 *
 *
 * refactor write_long_tag ?
 *
let level_incr = 2
let rec write_space ch level = 
  if level <= 0 then ()
  else (output_char ch ' '; write_space ch (level-1))
*)

open MapsSets

let foreword =
  [
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
  "<phyloxml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.phyloxml.org http://www.phyloxml.org/1.10/phyloxml.xsd\" xmlns=\"http://www.phyloxml.org\">";
  ]

let afterword = 
  [
  "</phyloxml>";
  ]

let write_string_list ch l = 
  List.iter (fun s -> Printf.fprintf ch "%s\n" s) l


(* generalized XML nonsense *)
let write_tag write_inners tag_name ch data = 
  Printf.fprintf ch "<%s>" tag_name;
  write_inners ch data;
  Printf.fprintf ch "</%s>\n" tag_name

let write_long_tag write_inners = 
  write_tag
    (fun ch data ->
      Printf.fprintf ch "\n";
      write_inners ch data)

let write_float = write_tag (fun ch x -> Printf.fprintf ch "%g" x)
let write_int = write_tag (fun ch x -> Printf.fprintf ch "%d" x)
let write_string = write_tag (fun ch x -> Printf.fprintf ch "%s" x)

(* writing tags *)
let write_decor_elt ch = function
  | Decor.Color(r,g,b) -> 
      write_long_tag
        (fun _ _ -> 
          write_int "red" ch r;
          write_int "green" ch g;
          write_int "blue" ch b;)
        "color"
        ch
        ()
  | Decor.Width w -> 
      write_float "width" ch w

let write_ftree_decor_at ch t id = 
  List.iter 
    (write_decor_elt ch)
    (Ftree.get_decoration_list t id)
  
let write_something_opt get_it write_it ch info id = 
  match get_it info id with
  | Some x -> write_it ch x
  | None -> ()

let write_taxon_opt = 
  write_something_opt 
    Itree_info.get_taxon_opt 
    (write_string "name")

let write_bl_opt = 
  write_something_opt 
    Itree_info.get_bl_opt 
    (write_float "branch_length")

(*
let write_boot_opt = 
  write_something_opt 
    Itree_info.get_boot_opt 
    (write_float "branch_length")
*)

let write_info ch info id = 
  List.iter 
    (fun f -> f ch info id)
    [ write_taxon_opt; write_bl_opt; ]

let write_ftree ch ftree = 
  let itree = Ftree.get_itree ftree in
  let info = Itree.get_info itree in
  let rec aux st = 
    let write_clade id tL =
      write_long_tag
        (fun _ _ -> 
          write_info ch info id; 
          write_ftree_decor_at ch ftree id;
          List.iter aux tL)
        "clade"
        ch
        ()
    in
    match st with
      | Stree.Node(id, tL) -> write_clade id tL
      | Stree.Leaf id -> write_clade id []
  in
  Printf.fprintf ch "<phylogeny rooted=\"true\">\n";
  aux (Itree.get_stree itree);
  Printf.fprintf ch "</phylogeny>\n\n"

let tree_list_to_file tl fname = 
  let ch = open_out fname in
  write_string_list ch foreword;
  List.iter (write_ftree ch) tl;
  write_string_list ch afterword;
  close_out ch
  
let tree_to_file t = tree_list_to_file [t]
