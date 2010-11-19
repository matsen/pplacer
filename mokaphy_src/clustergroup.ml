(* crazy script to group fat trees by cluster *)

let viz_dir = "/home/bvdiversity/working/matsen/complete/nontrivial"
(* let cluster_dir = "/home/bvdiversity/working/matsen/complete/cluster/tax"  *)
let cluster_dir = "/home/bvdiversity/working/matsen/complete/cluster/phy"
let cluster_fname = "/cluster.tre"
let mass_trees = "/mass_trees"

open MapsSets

let t = Newick.of_file (cluster_dir^cluster_fname)

let ssim = Clusterfind.ssim_of_tree t 

let subset i = StringSet.elements (IntMap.find i ssim)

let tree_stringl_of_fname fname = 
  File_parsing.string_list_of_file fname

let pad i = Printf.sprintf "%04d" i
let tree_name_of_int i = "/"^(pad i)^".tre.fat.xml"
let mass_name_of_int i = cluster_dir^mass_trees^(tree_name_of_int i)

let viz_name_of_str s = viz_dir^"/"^s^".xml"

let lines = File_parsing.string_list_of_file (mass_name_of_int 351)

let xml_rex = Str.regexp "xml"

let contains rex s = 
  try let _ = Str.search_forward rex s 0 in true with | Not_found -> false

let trees_of_fname fname = 
  List.filter 
    (fun s -> not (contains xml_rex s)) 
    (File_parsing.string_list_of_file fname)

let three_tree = trees_of_fname (mass_name_of_int 351)

let (xml_start_line, phyloxml_line) = 
  match File_parsing.string_list_of_file (mass_name_of_int 351) with
  | x::y::_ -> (x,y)
  | _ -> assert(false)

let xml_end_line = "</phyloxml>"

let build num = 
  let ch = open_out ((pad num)^"package.xml") in
  let write_line line = Printf.fprintf ch "%s\n" line in
  write_line xml_start_line;
  write_line phyloxml_line;
  List.iter write_line (trees_of_fname (mass_name_of_int num));
  List.iter 
    (fun s ->
      List.iter write_line (trees_of_fname (viz_name_of_str s)))
    (subset num);
  write_line xml_end_line;
  close_out ch
