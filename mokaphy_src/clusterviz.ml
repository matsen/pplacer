(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * A way to add names to internal nodes based on clustering results.
 *)

open MapsSets

exception Numbering_mismatch

let nameim_of_csv fname = 
  List.fold_right 
    (fun l -> 
      IntMap.add 
        (int_of_string (List.assoc "number" l)) 
        (List.assoc "name" l))
    (match (Csv.load fname) with 
    | h :: d -> Csv.associate h d
    | [] -> assert false)
    IntMap.empty 

(* makes a map from node labels (in bootstrap positions) to the node numbers *)
let nodemap_of_tree t = 
  IntMap.fold
    (fun i b -> 
      match b#get_boot_opt with
      | None -> fun m -> m
      | Some boot -> begin
          if boot <> float_of_int (int_of_float boot) then
            invalid_arg "non-integer label for a purported cluster tree";
          IntMap.add (int_of_float boot) i
        end)
    (Gtree.get_bark_map t)
    IntMap.empty

(* given a tree with node numbering in the bootstrap location, and a map from
 * those numbers to strings, naming those nodes, make a tree with names in the
 * appropriate locations and no bootstraps. *)
let make_named_tree sm t = 
  Gtree.set_bark_map t 
    (IntMap.map
      (fun b ->
        match b#get_boot_opt with
        | None -> b
        | Some boot ->
          let no_boot_b = b#set_boot_opt None
          and node_id = int_of_float boot in
          if not (IntMap.mem node_id sm) then no_boot_b
          else no_boot_b#set_name (IntMap.find node_id sm))
      (Gtree.get_bark_map t))

let build_name_tree dirname nameim = 
  let t = Newick.of_file (Cluster_common.tree_name_of_dirname dirname) in
  let nodeim = nodemap_of_tree t in
  (* shifted_nameim uses the numbering within the tree rather than that given by
   * the labels (as nameim does) *)
  let shifted_nameim = 
    try
      IntMap.fold
        (fun cluster_num name -> 
          Printf.printf "%d\t%s\n" cluster_num name;
          IntMap.add (IntMap.find cluster_num nodeim) name)
        IntMap.empty
        nameim
    with
    | Not_found -> raise Numbering_mismatch
  in
  make_named_tree shifted_nameim t
