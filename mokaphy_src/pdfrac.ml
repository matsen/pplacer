(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Here we do pdfrac. 
 *
 * Imagine that each sample is associated with a color, and that we color the
 * tree with each color individually according to the induced tree on that
 * sample.
 * Programmaticially this works as follows: for each color we have a boolean,
 * and when we traverse the tree this bool gets 
 *
 * Another implementation would be to use integer sets to keep track of the
 * colors. 
 *)

open MapsSets
open Fam_batteries
open BitfieldMaps


(* here we go! *)


(* get map from int to bf representing samples on that edge *)
let bf_intmap_of_inda inda = 
  let m = ref IntAlgMapBf.M.empty in 
  for i=0 to (Array.length inda)-1 do
    let ei = Bitfield.ei i in
    IntMap.iter
      (fun loc _ -> m := IntAlgMapBf.max_by loc ei !m)
      inda.(i);
  done;
  !m



  (*
let bf_intmap_of_inda inda = 
  let m = ref BFAMR.M.empty in 
  for i=0 to (Array.length inda)-1 do
    let ei = Bitfield.ei i in
    IntMap.iter
      (fun loc distal ->
        m := IntMapFuns.add_listly loc {bf=ei; distal=distal} !m)
      inda.(i);
  done;
  !m

  let _ = 
    Gtree.recur
      (fun id below -> 
        assert(below <> []);
        process_edge id
          (List.fold_left (lor) (List.hd below) (List.tl below)))
      (fun id -> process_edge id Bitfield.empty)
  in
  m



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

(* later have a lazy data cache with the induceds? *)
let pd_of_pr criterion pr = 
  pd_of_induced (Placerun.get_ref_tree pr) 
                (Induced.induced_of_placerun criterion pr)


*)
