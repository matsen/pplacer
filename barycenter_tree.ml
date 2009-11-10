(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let make weighting criterion p pr_arr =
  let bary_map = 
    IntMapFuns.of_pairlist_listly
      (Array.to_list
        (Array.map 
          (Barycenter.of_placerun weighting criterion p)
          pr_arr))
  in
  let ref_tree = 
    let ref_trees = Array.map Placerun.get_ref_tree pr_arr in
    for i=1 to (Array.length pr_arr)-1 do
      assert(0 = Newick.compare ref_trees.(0) ref_trees.(i))
    done;
    ref_trees.(0)
  in
  Gtree.add_subtrees_by_map
    (fun bl -> print_endline "hi"; Placeviz_core.decor_bark_of_bl bl)
    (Decor_gtree.of_newick_gtree ref_tree)
    (IntMap.map
      (List.map
        (fun pos -> (pos, Gtree.Internal_node)))
      bary_map)

let write weighting criterion p bary_prefix pr_arr =
  Placeviz_core.trees_to_file
    Placeviz_core.Phyloxml
    bary_prefix
    [ make weighting criterion p pr_arr ]
