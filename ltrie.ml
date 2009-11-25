(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * a trie of same-length lists with storage of things
 * what about repeats
*)


open MapsSets

exception Wrong_length

type 'a t = 
  | Middle of ('a t) IntMap.t
  | Terminus of 'a

type 'a approximate_choice = int * int list -> int

let is_terminus = function
  | Middle _ -> false
  | Terminus _ -> true

let rec mem t = function
  | [] -> if is_terminus t then true 
          else raise Wrong_length
  | x::l ->
      match t with
      | Middle m ->
          if IntMap.mem x m then mem (IntMap.find x m) l
          else false
      | Terminus _ -> raise Wrong_length

let add t k y = 
  let rec aux t = function
    | [] -> Terminus y
    | x::l ->
        match t with
        | Middle m ->
            Middle 
              (IntMap.add
                x
                (aux (IntMap.find x m) l)
              our_map
        | Terminus _ -> raise Wrong_length
  in
  aux t k
          


(* ppr *)

let rec ppr ppr_k ppr_v ff = function
  | Middle h ->
      Format.fprintf ff "@[{";
        Ppr.ppr_list_inners 
            (fun ff k ->
              Format.fprintf ff "%a -> @[%a@]"
                ppr_k k
                (ppr ppr_k ppr_v) (IntMap.find h k))
            ff
            (IntMapFuns.keys h);
      Format.fprintf ff "}@]";
  | Terminus yl ->
      (* Ppr.ppr_list ppr_v ff yl *)
      ppr_v ff y

