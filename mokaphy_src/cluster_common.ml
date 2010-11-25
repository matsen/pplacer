(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * A way to add names to internal nodes based on clustering results.
 *)

open MapsSets

module StringSetSet = 
  Set.Make(struct type t = StringSet.t let compare = StringSet.compare end)

let cluster_tree_name = "/cluster.tre"
let mass_trees_dirname = "mass_trees"

let tree_name_of_dirname dirname = dirname^cluster_tree_name

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
 * map from boot value to sets below.
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

let sss_of_tree t = 
  IntMap.fold (fun _ s -> StringSetSet.add s) (ssim_of_tree t)
    StringSetSet.empty
