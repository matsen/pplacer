open Ppatteries
open Myxml

let phyloxml_ns = "http://www.phyloxml.org"
let xsi_ns = "http://www.w3.org/2001/XMLSchema-instance"
let nsify s = phyloxml_ns, s

let root_tag =
  nsify "phyloxml",
  [(Xmlm.ns_xmlns, "xmlns"), phyloxml_ns;
   (Xmlm.ns_xmlns, "xsi"), xsi_ns;
   (xsi_ns, "schemaLocation"), "http://www.phyloxml.org/1.10/phyloxml.xsd"]

let ns_prefix = function
  | x when x = phyloxml_ns -> Some ""
  | x when x = xsi_ns -> Some "xsi"
  | _ -> None

let rec emit_tag out {name; attrs; contents; children} =
  out (`El_start (nsify name, List.map (first nsify) attrs));
  if contents <> "" then out (`Data contents);
  List.iter (emit_tag out) children;
  out `El_end

let rec emit_stree out bark_map stree =
  out (`El_start (nsify "clade", []));
  let top = Stree.top_id stree in
  IntMap.Exceptionless.find top bark_map
    |> Option.may (fun b -> List.iter (emit_tag out) b#to_xml);
  begin match stree with
    | Stree.Node (_, subtrees) -> List.iter (emit_stree out bark_map) subtrees
    | _ -> ()
  end;
  out `El_end

let emit_gtree out (name, gtree) =
  out (`El_start (nsify "phylogeny", [nsify "rooted", "true"]));
  Option.may (tag "name" |- emit_tag out) name;
  emit_stree out (Gtree.get_bark_map gtree) (Gtree.get_stree gtree);
  out `El_end

let named_gtrees_to_output l out =
  out (`Dtd None);
  out (`El_start root_tag);
  List.iter (emit_gtree out) l;
  out `El_end

let output_of_channel ch =
  Xmlm.make_output ~ns_prefix (`Fun (IO.write_byte ch))

let named_gtrees_to_channel ch l =
  output_of_channel ch
    |> Xmlm.output
    |> named_gtrees_to_output l

let named_gtrees_to_file fname l =
  open_out fname
    |> with_dispose ~dispose:close_out (flip named_gtrees_to_channel l)

let named_gtree_to_file ~fname ~tree_name t =
  named_gtrees_to_file fname [Some tree_name, t]
let gtrees_to_file fname l =
  named_gtrees_to_file fname (List.map ((const None) &&& identity) l)
let gtree_to_file fname t =
  named_gtrees_to_file fname [None, t]
