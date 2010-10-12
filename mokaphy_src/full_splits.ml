(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open MapsSets
open Fam_batteries
open Stree


type full_split = 
  {
    prox : int list;
    dist : int list;
  }

let ppr ff s = 
  let ppil = Ppr.ppr_list_inners Format.pp_print_int in
  Format.fprintf ff "@[{%a | %a}@]" ppil s.prox ppil s.dist

let make_distal_map t = 
  let m = ref IntMap.empty in
  let add id l = m := IntMapFuns.check_add id l !m in
  let _ = 
    Stree.recur
      (fun id below -> 
        let below_cat = List.concat below in
        add id below_cat;
        id::below_cat)
      (fun id -> add id []; [id])
      t
  in
  IntMap.map (List.sort compare) !m

let make_proximal_map distal_map t = 
  let m = ref IntMap.empty in
  let add id l = m := IntMapFuns.check_add id l !m in
  let rec aux above = function
    | Node(id, tL) ->
        List.iter
          (fun (out, rest) ->
            let union = 
              List.concat
                ((id::above)::
                  (List.map 
                    (fun t' -> 
                      (top_id t')::(IntMap.find (top_id t') distal_map))
                    rest))
            in
            add (top_id out) union;
            aux union out)
          (Base.pull_each_out tL);
    | Leaf _ -> () (* already taken care of by above *)
  in
  let () = try aux [] t with
           | Not_found -> assert(false)
  in
  IntMap.map (List.sort compare) !m

let full_splits t = 
  let d = make_distal_map t in
  let p = make_proximal_map d t in
  (* fold over proximal map because it doesn't include the root *)
  IntMap.fold 
    (fun id prox -> IntMap.add id {prox=prox;dist=(IntMap.find id d);})
    p
    IntMap.empty
