(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

let cluster_tree_name = "/cluster.tre"
let named_cluster_tree_name = "/named_cluster.tre"

type stats = 
  {
    size : int;
    factor : float;
    ind1 : int;
    ind2 : int;
    name : string;
  }

let stats_of_strl = function
  | [ sizes; factors; ind1s; ind2s; names ] ->
      {
        size = int_of_string sizes;
        factor = float_of_string factors;
        ind1 = int_of_string ind1s;
        ind2 = int_of_string ind2s;
        name = names;
      }
  | _ -> failwith "clusterviz expects five inputs per line in the cluster file"

let whitespace_rex = Str.regexp "[ \t]+"

let stats_of_line line = 
  stats_of_strl (Str.split whitespace_rex line)

let read_cluster_stats fname = 
  let lines = File_parsing.string_list_of_file fname in
  List.map stats_of_line lines

(* we assume that the tree is labeled in the bootstrap positions *)
let nodemap_of_tree t = 
  IntMap.fold
    (fun i b -> 
      match b#get_boot_opt with
      | None -> fun m -> m
      | Some boot -> IntMap.add (int_of_float boot) i)
    (Gtree.get_bark_map t)
    IntMap.empty

let statmap_of_statl index_of_stat statl = 
  List.fold_right (fun s -> IntMap.add (index_of_stat s) s) statl IntMap.empty 

let make_name_bark sm t = 
  Gtree.set_bark_map t 
    (IntMap.map
      (fun b ->
        match b#get_boot_opt with
        | None -> b
        | Some boot ->
          let node_id = int_of_float boot in
          if not (IntMap.mem node_id sm) then b#set_boot_opt None
          else b#set_name (IntMap.find node_id sm).name)
      (Gtree.get_bark_map t))

let clusterviz cluster_file dirname1 dirname2 = 
  let t1 = Newick.of_file (dirname1^cluster_tree_name)
  and t2 = Newick.of_file (dirname2^cluster_tree_name)
  and statl = read_cluster_stats cluster_file
  in
  let sm1 = statmap_of_statl (fun s -> s.ind1) statl
  and sm2 = statmap_of_statl (fun s -> s.ind2) statl
  in
  Newick.to_file (make_name_bark sm1 t1) (dirname1^named_cluster_tree_name);
  Newick.to_file (make_name_bark sm2 t2) (dirname2^named_cluster_tree_name);
  ()

