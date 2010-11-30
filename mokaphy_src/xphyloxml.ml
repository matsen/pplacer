(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * This module is definitely the future of phyloxml support in pplacer, but
 * isn't fully integrated yet.
 *)

exception Multiple_root_clades

let phyloxml_attrs = 
  [
    "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance";
    "xsi:schemaLocation", "http://www.phyloxml.org/1.10/phyloxml.xsd";
    "xmlns", "http://www.phyloxml.org";
  ]

type pxtree = 
  {
    name: string option;
    clade: Xml.xml;
    attribs: (string * string) list;
  }

type pxdata = 
  {
    trees: pxtree list;
    data_attribs: (string * string) list;
  }

let assert_phylogeny t = assert("phylogeny" = Xml.tag t)
let assert_clade x = assert("clade" = Xml.tag x)

let pcdata_inners_of_xml x = 
  match Xml.children x with
  | [pcd] -> Xml.pcdata pcd
  | _ -> assert false

let pxtree_of_xml x = 
  assert_phylogeny x;
  match Xml.children x with
  | [namex; clade] ->
      assert("name" = Xml.tag namex);
      assert_clade(clade);
      { name = Some (pcdata_inners_of_xml namex); 
        clade = clade; 
        attribs = Xml.attribs x; }
  | [clade] -> 
      assert_clade(clade);
      { name = None; clade = clade; attribs = Xml.attribs x; }
  | _ -> raise Multiple_root_clades

let xml_of_pxtree t = 
  (* NOTE: assume no attribs of the root clade *)
  let xclade = Xml.Element("clade", [], [t.clade]) in
  let children = 
    match t.name with
    | None -> [xclade]
    | Some n -> [Xml.Element("name",[], [Xml.PCData n]); xclade]
  in
  Xml.Element("phylogeny", t.attribs, children)

let load fname = 
  let full = Xml.parse_file fname in
  assert("phyloxml" = Xml.tag full);
  {
    trees = List.map pxtree_of_xml (Xml.children full);
    data_attribs = Xml.attribs full;
  }

let xml_of_pxdata pxd = 
  Xml.Element("phyloxml", pxd.data_attribs, List.map xml_of_pxtree pxd.trees)

let pxdata_to_file fname pxd = 
  let ch = open_out fname in
  Printf.fprintf ch "%s\n" (Xml.to_string_fmt (xml_of_pxdata (pxd)));
  close_out ch

let echo fname =
  print_endline (Xml.to_string_fmt (xml_of_pxdata (load fname)))
