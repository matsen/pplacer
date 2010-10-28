(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for gtrees which have a Decor bark map.
*)

open Fam_batteries
open MapsSets


let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2

let of_newick_gtree t = Gtree.map_bark_map Decor_bark.of_newick_bark t
let to_newick_gtree t = Gtree.map_bark_map Decor_bark.to_newick_bark t

(* decor_map is an IntMap to a Decor.decoration list *)
let add_decor_by_map t decor_map = 
  let bark_map = Gtree.get_bark_map t in
  Gtree.set_bark_map t
    (IntMap.fold
      (fun id decor_list ->
        IntMap.add 
          id
          ((if IntMap.mem id bark_map then IntMap.find id bark_map 
          else new Decor_bark.decor_bark `Empty)
            #append_decor decor_list))
      decor_map
      bark_map)


