open MapsSets

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
  let children =
    match t.name with
    | None -> []
    | Some n -> [Myxml.tag "name" n]
  in
  Xml.Element("phylogeny", t.attribs, children @ [t.clade])

let load fname =
  let full = Xml.parse_file fname in
  assert("phyloxml" = Xml.tag full);
  {
    trees = List.map pxtree_of_xml (Xml.children full);
    data_attribs = Xml.attribs full;
  }

let xml_of_pxdata pxd =
  Xml.Element("phyloxml", phyloxml_attrs @ pxd.data_attribs, List.map xml_of_pxtree pxd.trees)

let pxdata_to_channel ch pxd =
  Printf.fprintf ch "%s\n" (Xml.to_string_fmt (xml_of_pxdata (pxd)))

let pxdata_to_file fname pxd =
  let ch = open_out fname in
  pxdata_to_channel ch pxd;
  close_out ch

let echo fname =
  print_endline (Xml.to_string_fmt (xml_of_pxdata (load fname)))

let rec clade_of_stree bark tree =
  let tags id =
    if IntMap.mem id bark
    then (IntMap.find id bark)#to_xml
    else [] in
  let id, children = match tree with
  | Stree.Node (id, tL) -> id, tL
  | Stree.Leaf id -> id, [] in
  Xml.Element ("clade", [], tags id @ List.map (clade_of_stree bark) children)

let pxtree_of_gtree ?name gtree =
  {
    name = name;
    clade = clade_of_stree (Gtree.get_bark_map gtree) (Gtree.get_stree gtree);
    attribs = [("rooted", "true")];
  }

let pxdata_of_gtrees gtrees =
  {
    trees = List.map pxtree_of_gtree gtrees;
    data_attribs = [];
  }

let pxdata_of_named_gtrees gtrees =
  {
    trees = List.map (fun (name, tree) -> pxtree_of_gtree ?name tree) gtrees;
    data_attribs = [];
  }

let pxdata_of_gtree t = pxdata_of_gtrees [t]
let pxdata_of_named_gtree name t = pxdata_of_named_gtrees [Some name, t]

let gtree_to_file fname t = pxdata_to_file fname (pxdata_of_gtree t)
let gtrees_to_file fname l = pxdata_to_file fname (pxdata_of_gtrees l)
let named_gtree_to_file ~fname ~tree_name t =
  pxdata_to_file fname (pxdata_of_named_gtree tree_name t)
let named_gtrees_to_file fname l =
  pxdata_to_file fname (pxdata_of_named_gtrees l)
