(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let empty = IntMap.empty
let add = IntMap.add
let find = IntMap.find
let mem = IntMap.mem
let union = IntMapFuns.union

let ppr ff bm = 
  IntMapFuns.ppr_gen (fun ff bark -> bark#ppr ff) ff bm

let boost by m = 
  IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty

let to_name_map bm = 
  IntMap.fold
    (fun id bark accu ->
      try 
        IntMap.add id bark#get_name accu
      with
      | Newick_bark.No_name -> accu)
    bm
    IntMap.empty

