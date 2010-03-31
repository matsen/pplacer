(* mokaphy v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let make weighting criterion p pr_arr =
  let bary_map = 
    IntMapFuns.of_pairlist_listly
      (Array.to_list
        (Array.mapi
          (fun i pr ->
            let (loc, pos) = 
              Barycenter.of_placerun weighting criterion p pr in
            (loc,
              (pos, 
              Gtree.Internal_node,
              (fun bl -> 
                new Decor_bark.decor_bark 
                  (`Of_bl_name_boot_dlist 
                     (Some bl, None, None, [Decor.dot i]))))))
          pr_arr))
  in
  (* we don't use get_same_tree here because it's a whole array *)
  let ref_tree = 
    let ref_trees = Array.map Placerun.get_ref_tree pr_arr in
    for i=1 to (Array.length pr_arr)-1 do
      if 0 <> Newick.compare ref_trees.(0) ref_trees.(i) then
        failwith("barycenter calculation: not all reference trees are the same!");
    done;
    ref_trees.(0)
  in
  Gtree.add_subtrees_by_map
    (Decor_gtree.of_newick_gtree ref_tree)
    bary_map

let write weighting criterion p bary_prefix pr_arr =
  Placeviz_core.trees_to_file
    Placeviz_core.Phyloxml
    (bary_prefix^".bary")
    [ make weighting criterion p pr_arr ]
