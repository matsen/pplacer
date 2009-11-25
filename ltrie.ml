(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * do we want to have a "count", could have a combine_data
*)


open MapsSets

type 'a t = { data : 'a list; node : ('a t) IntMap.t }

type 'a approximate_choice = int * int list -> int

let empty = { data = []; node = IntMap.empty; }

let rec mem t = function
  | [] -> true 
  | x::l ->
      if IntMap.mem x t.node then mem (IntMap.find x t.node) l
      else false

let rec add k y t = 
  match k with
  | [] -> { t with data = y::t.data }
  | x::l ->
      { t with node = 
        IntMap.add
          x
          (add l y
            (if IntMap.mem x t.node then IntMap.find x t.node
            else { data = []; node = IntMap.empty }))
          t.node }


(* ppr *)

let rec ppr ppr_v ff t = 
  Format.fprintf ff "@[{";
  Format.fprintf ff "@[data = %a; @]" (Ppr.ppr_list ppr_v) t.data;
    Ppr.ppr_list_inners 
     (fun ff k ->
       Format.fprintf ff "%a -> @[%a@]"
         Format.pp_print_int k
         (ppr ppr_v) (IntMap.find k t.node))
     ff
     (IntMapFuns.keys t.node);
  Format.fprintf ff "}@]"

let ppr_int = ppr Format.pp_print_int
