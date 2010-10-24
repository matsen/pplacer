(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets


let hashtbl_find_zero h k = if Hashtbl.mem h k then Hashtbl.find h k else 0.

(* here we build the mass map which is appropriate for the tax_gtree. ti_imap
 * takes us from the locations on the tree to taxids *)
let edgem tax_id_of_place criterion ti_imap pr =  
  (* guess that the number of occupied taxids is one third of the number of
   * nodes in the reference tree *)
  let h = Hashtbl.create ((IntMapFuns.nkeys ti_imap)/3) in
  let addto ti x = Hashtbl.replace h ti (x+.(hashtbl_find_zero h ti)) in
  List.iter
    (fun pq ->
      List.iter 
        (fun p -> addto (tax_id_of_place p) (criterion p))
        (Pquery.place_list pq))
    (Placerun.get_pqueries pr);
  Mass_map.By_edge.normalize_mass (IntMap.map (hashtbl_find_zero h) ti_imap)

let to_bottm weight = (0., weight)

(* just get a MassMap.Indiv version of the map with all of the mass on the
 * bottom of the edge *)
let bottm tax_id_of_place criterion ti_imap pr = 
  IntMap.map to_bottm (edgem tax_id_of_place criterion ti_imap pr)
