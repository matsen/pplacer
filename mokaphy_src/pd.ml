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
let pd_of_induced t ind = 
  (* start recording the total branch length from the most distal placement *)
  let perhaps_start_path id =
    match IntMapFuns.opt_find id ind with
    | None -> None (* no path *)
    | Some x -> Some ((Gtree.get_bl t id) -. x)
  in
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
  | Some _ -> !total
  | None -> failwith "empty induced tree"

(* later have a lazy data cache with the induceds? *)
let pd_of_pr criterion pr = 
  pd_of_induced (Placerun.get_ref_tree pr) 
                (Induced.induced_of_placerun criterion pr)


module OrderedBitfield = struct
  type t = Bitfield.t
  let compare = Bitfield.compare
end

module StringableBitfield = struct
  type t = Bitfield.t
  let to_string = Bitfield.to_string
end

module BFAMR = AlgMap.AlgMapR(OrderedBitfield)

(* our strategy here is to attach bitfields representing the colors on the edges
 * (each color represents a subset of the samples) to the tree. at the
 * beginning, each color is a unit vector at the most distal location on the
 * tree for that sample.
 *
 * a pos_bf is a bitfield along with a distal branch length for the edge that it
 * is connected to.
*)
type bfd = 
  {
    bf      :  Bitfield.t;
    distal  :  float;
  }

let ncompare_by_distal a b = - compare a.distal b.distal

let bfdl_intmap_of_inda inda = 
  let m = ref IntMap.empty in 
  for i=0 to (Array.length inda)-1 do
    let ei = Bitfield.ei i in
    IntMap.iter
      (fun loc distal ->
        m := IntMapFuns.add_listly loc {bf=ei; distal=distal} !m)
      inda.(i);
  done;
  !m

(* we put in a bfim from the previous step and get out a map from the various
 * color combinations to the amount of branch length corresponding to that color
 * combination *)
let make_bfmap t bfim = 
  let m = ref BFAMR.M.empty in
    let add_snip bf snip_len =
      assert(snip_len >= 0.);
      m := BFAMR.add_by bf snip_len !m;
    in
  (* add the bf to the AlgMap and return start_bf union all of the bfs along the
   * edge *)
  let process_edge id start_bf = 
    let bl = Gtree.get_bl t id in
    let final_bfd = 
      List.fold_left
        (fun bfd prev_bfd ->
          assert(bl >= bfd.distal);
          (* add current up to the new location *)
          add_snip prev_bfd.bf (bfd.distal -. prev_bfd.distal);
          (* "or" to make the next bf *)
          {bfd with bf = (bfd.bf lor prev_bfd.bf)})
        {bf=start_bf; distal=0.}
        (List.sort ncompare_by_distal 
          (try IntMap.find id bfim with | Not_found -> []))
    in
    add_snip final_bfd.bf (bl -. final_bfd.distal);
    final_bfd.bf
  in
  let _ = 
    Gtree.recur
      (fun id below -> 
        assert(below <> []);
        process_edge id
          (List.fold_left (lor) (List.hd below) (List.tl below)))
      (fun id -> process_edge id Bitfield.empty)
  in
  m
