(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets


(* *** classification *** *)
let add_classif what how p = what p (how p)

let classify_pq what how pq = 
  { pq with Pquery.place_list = 
    List.map (add_classif what how) pq.Pquery.place_list }

let classify_pr what how pr = 
  Placerun.set_pqueries pr
    (List.map (classify_pq what how) (Placerun.get_pqueries pr))

(* classification types *)
let containment_classify mrcam utm p = 
  let rec aux i = 
    if IntMap.mem i mrcam then IntMap.find i mrcam
    else aux (IntMap.find i utm)
  in
  try aux (Placement.location p) with
  | Not_found -> Tax_id.NoTax

(* applied to classification types *)
let refpkg_contain_classify rp pr = 
  classify_pr 
    Placement.add_contain_classif
    (containment_classify 
      (Refpkg.get_mrcam rp) 
      (Refpkg.get_uptree_map rp))
    pr

