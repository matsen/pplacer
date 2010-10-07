(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * assumes that the bark is at least a newick bark
*)

open MapsSets

let union = IntMapFuns.union

let ppr ff bm = 
  IntMapFuns.ppr_gen (fun ff bark -> bark#ppr ff) ff bm

let boost by m = 
  IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty

let get_bl m id = (IntMap.find id m)#get_bl
let get_name m id = (IntMap.find id m)#get_name
let get_boot m id = (IntMap.find id m)#get_boot

let to_name_map bm = 
  IntMap.fold
    (fun id bark accu ->
      try 
        IntMap.add id bark#get_name accu
      with
      | Newick_bark.No_name -> accu)
    bm
    IntMap.empty

