(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * a trie of same-length lists with storage of things
 * what about repeats
*)

let initial_size = 5

type ('a, 'b) t = 
  | Middle of ('a, ('a,'b) t) Hashtbl.t
  | Terminus of 'b

type 'a approximate_choice = 'a * 'a list -> 'a

let rec mem t = function
  | [] -> true
  | x::l ->
      match t with
      | Middle h ->
          if Hashtbl.mem h x then mem (Hashtbl.find h x) l
          else false
      | Terminus _ -> false

let new_middle () = 
  Middle (Hashtbl.create initial_size)

let add lt k y = 
  let rec aux t = function
    | [] -> Terminus y
    | x::l ->
        match t with
        | Middle h ->
            if Hashtbl.mem h x then add (Hashtbl.find h x) l
            else Hashtbl.add h x (aux (new_middle ()) l)
        | Terminus _ -> ()
  in
  aux lt k
          


(* ppr *)

let get_keys h = Hashtbl.fold (fun k _ accu -> k::accu) h []

let rec ppr ppr_k ppr_v ff = function
  | Middle h ->
      Format.fprintf ff "@[{";
        Ppr.ppr_list_inners 
            (fun ff k ->
              Format.fprintf ff "%a -> @[%a@]"
                ppr_k k
                (ppr ppr_k ppr_v) (Hashtbl.find h k))
            ff
            (get_keys h);
      Format.fprintf ff "}@]";
  | Terminus yl ->
      Ppr.ppr_list ppr_v ff yl

