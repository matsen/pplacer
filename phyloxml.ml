(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * can write any gtree whose bark has a write_xml method.
*)

open Fam_batteries
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

let write_string_list ch = 
  List.iter (fun s -> Printf.fprintf ch "%s\n" s)

(* output *)

let write_tree ?name ch gtree = 
  let bark_map = Gtree.get_bark_map gtree in
  let rec aux st = 
    let write_clade id tL =
      Xml.write_long_tag
        (fun () -> 
          (* write the bark *)
          if IntMap.mem id bark_map then
            (IntMap.find id bark_map)#write_xml ch;
          (* write the id *)
          Xml.write_long_tag
            (fun () -> Xml.write_int "id" ch id)
            "taxonomy"
            ch;
          List.iter aux tL)
        "clade"
        ch
    in
    match st with
      | Stree.Node(id, tL) -> write_clade id tL
      | Stree.Leaf id -> write_clade id []
  in
  Printf.fprintf ch "<phylogeny rooted=\"true\">\n";
  match name with
  | None -> ()
  | Some n -> Xml.write_string "name" ch n;
  aux (Gtree.get_stree gtree);
  Printf.fprintf ch "</phylogeny>\n\n"

let tree_list_to_file tl fname = 
  let ch = open_out fname in
  write_string_list ch foreword;
  List.iter (write_tree ch) tl;
  write_string_list ch afterword;
  close_out ch

(* tl should be a list of (name_opt, t). thus not all trees have to have a name *)
let named_tree_list_to_file tl fname = 
  let ch = open_out fname in
  write_string_list ch foreword;
  List.iter 
    (fun (name_opt, t) ->
      write_tree ?name:name_opt ch t) 
    tl;
  write_string_list ch afterword;
  close_out ch
  
let tree_to_file t = tree_list_to_file [t]
let named_tree_to_file tname t = named_tree_list_to_file [Some tname, t]
