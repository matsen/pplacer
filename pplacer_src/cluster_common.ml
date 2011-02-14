(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Note that for much of this clustering work we assume that trees have
 * numberings in their internal nodes, where the bootstrap values go.
 *)

open MapsSets

module StringSetSet =
  Set.Make(struct type t = StringSet.t let compare = StringSet.compare end)

let cluster_tree_name = "cluster.tre"
let mass_trees_dirname = "mass_trees"

let path_join_two a b = if a.[(String.length a)-1] = '/' then a^b else a^"/"^b
let rec path_join = function [] -> "" | h::t -> path_join_two h (path_join t)

let tree_name_of_dirname dirname = path_join_two dirname cluster_tree_name

let check_add x s =
  if StringSet.mem x s then invalid_arg "check_add"
  else StringSet.add x s

let disj_union s1 s2 =
  if StringSet.empty = StringSet.inter s1 s2 then
    StringSet.union s1 s2
  else
    invalid_arg "disj_union"

let list_disj_union = List.fold_left disj_union StringSet.empty

(*
 * Make a map from boot value to sets below.
 * note that we only store non-singletons.
 * *)
let ssim_of_tree t =
  let m = ref IntMap.empty in
  let my_add k v = m := IntMap.add k v !m in
  let rec aux = function
    | Stree.Node(id, tL) ->
        let below = list_disj_union (List.map aux tL) in
        my_add (int_of_float (Gtree.get_boot t id)) below;
        below
    | Stree.Leaf(id) ->
        StringSet.singleton (Gtree.get_name t id)
  in
  let _ = aux (Gtree.get_stree t) in
  !m

(* put in boot numbers into the bootstrap vals for functions that use it *)
let number_tree t =
  Gtree.set_bark_map t
    (IntMap.mapi
    (fun i b -> b#set_boot (float_of_int i))
    (Gtree.get_bark_map t))

(* make sure that there are boot numberings everywhere. if not, then adopt
 * them a la number_tree. *)
let ensure_numbered t =
  try let _ = ssim_of_tree t in t with
  | Newick_bark.No_boot ->
      print_endline "Warning: inserting numbers into internal edges.";
      number_tree t

(* In a rooted tree, each internal node gives a taxon set, which is the set of
 * taxa below that node. Here we collect the set of such (non-singleton) sets.
 * We use number_tree because the internal numbering scheme doesn't matter here. *)
let sss_of_tree t =
  IntMap.fold
    (fun _ s -> StringSetSet.add s)
    (ssim_of_tree (number_tree t))
    StringSetSet.empty

(* fname has two columns, "number", which has the nodes of the internal nodes of
 * the tree, and "name", which has names for those internal nodes *)
let numnamel_of_csv fname =
  try
    List.map
      (fun l ->
        int_of_string (List.assoc "number" l),
        List.assoc "name" l)
      (match (Csv.load fname) with
      | h :: d -> Csv.associate h d
      | [] -> assert false)
  with
  | Not_found ->
      failwith ("Couldn't find 'name' and 'number' column headers in "^fname)

let nameim_of_csv fname =
  List.fold_right
    (fun (i,n) -> IntMap.add i n)
    (numnamel_of_csv fname)
    IntMap.empty
