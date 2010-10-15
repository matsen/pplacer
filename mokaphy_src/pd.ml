(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open MapsSets
open Fam_batteries

(* returns if at least two of the descendants have things in them *)
let total_floatol =
  List.fold_left (fun x -> function | Some y -> x+.y | None -> x) 0.

(* compute the PD of an induced tree. we recursively go through the tree,
 * returning Some len if there is a placement distal to us, with len being the
 * length of the path to the last recorded MRCA. *)
let of_induced t ind = 
  (* start recording the total branch length from the most distal placement *)
  let perhaps_start_path id =
    match IntMapFuns.opt_find id ind with
    | None -> None (* no path *)
    | Some x -> Some ((Gtree.get_bl t id) -. x)
  in
  (* when we hit an MRCA, we add on the branch lengths here *)
  let total = ref 0. in
  let add_to_tot x = total := x +. !total in
  match
    Gtree.recur
      (fun id below ->
        match List.filter ((<>) None) below with
        | [] -> perhaps_start_path id
        | [Some x] -> Some ((Gtree.get_bl t id) +. x) (* continue path *)
        | _ as l -> 
            add_to_tot (total_floatol l); (* record lengths from distal paths *)
            Some (Gtree.get_bl t id)) (* start recording from mrca of those paths *)
      perhaps_start_path
      t
  with
  | Some x -> x +. !total (* add on the last bit of path *)
  | None -> failwith "empty induced tree"

(* later have a lazy data cache with the induceds? *)
let of_pr criterion pr = 
  of_induced (Placerun.get_ref_tree pr) 
                (Induced.of_placerun criterion pr)

