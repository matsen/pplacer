(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Here we do pdfrac. 
 *
 * Imagine that each sample is associated with a color, and that we color the
 * tree with each color individually according to the induced tree on that
 * sample.
 * We are interested in getting a map such that for each subset of colors, we
 * get the total branch length on the tree which has exactly those colors along
 * that edge.
 * We call this a pdfrac map, because it's the primary storage for what we are
 * interested in.
 * 
 * We record the color sets with bitfields. 
 * Thus pdfrac maps are float Bitfield maps, in fact their algebraic versions, 
 * module BFAMR = AlgMapR(OrderedBitfield)
 *
 * In order to make those easily, we calculate the color sets which are
 * represented on either side of each edge, i.e. the distal and proximal bfims.
 *
 * Abbreviations:
 *   inda is an induced array
 *   bfim is a Bitfield.t IntMap
 *
 * 
 *
 * Another implementation would be to use integer sets to keep track of the
 * colors. 
 *)

open MapsSets
open Fam_batteries
open BitfieldMaps

(* hbf: height bitfield. this is the essential element of on-the-edge
 * calculation *)
type hbf = 
  {
    ht : float;
    bf : Bitfield.t;
  }

let ncompare_by_height a b = - compare a.ht b.ht

(* the hbf list IntMap of samples on each edge *)
let hbflim_of_inda inda = 
  let m = ref IntMap.empty in 
  for i=0 to (Array.length inda)-1 do
    let ei = Bitfield.ei i in
    IntMap.iter
      (fun loc ht -> 
        m := IntMapFuns.add_listly loc {ht=ht; bf=ei} !m)
      inda.(i);
  done;
  !m

let find_listly i m = try IntMap.find i m with | Not_found -> []

let make_distal_bfim hbflim t = 
  let m = ref IntMap.empty in
  let add id bf = m := IntMapFuns.check_add id bf !m in
  let bfget id = 
    match List.map (fun hbf -> hbf.bf) (find_listly id hbflim) with
    | [] -> Bitfield.empty
    | x::l -> List.fold_left (lor) x l
  in
  let _ = 
    Gtree.recur
      (fun id below -> 
        let below_tot = 
          match below with
          | hd::tl -> List.fold_left (lor) hd tl
          | [] -> assert(false)
        in
        add id below_tot;
        (bfget id) lor below_tot)
      (fun id -> add id Bitfield.empty; bfget id)
      t
  in
  !m

let make_proximal_bfim distal_bfim t = 
  let m = ref IntMap.empty in
  let add k v = m := IntMapFuns.check_add k v !m in
  let rec aux above = function
    | Stree.Node(_, tL) ->
        List.iter
          (fun (out, rest) ->
            let union = 
              List.fold_left 
                (lor) 
                above
                (List.map 
                  (fun rid -> IntMap.find rid distal_bfim) 
                  (List.map Stree.top_id rest))
            in
            add (Stree.top_id out) union;
            aux union out)
          (Base.pull_each_out tL);
    | Stree.Leaf _ -> ()
  in
  let () = try aux Bitfield.empty (Gtree.get_stree t) with
           | Not_found -> assert(false)
  in
  !m


(* we put in a bfim from the previous step and get out a map from the various
 * color combinations to the amount of branch length corresponding to that color
 * combination *)
let make_fbfm t inda = 
  let m = ref BFAMR.M.empty in
  let add_snip ~prox ~dist =
    Printf.printf "%s\t%s\t%f\t%f\n"
      (Bitfield.to_string prox.bf)
      (Bitfield.to_string dist.bf)
      prox.ht
      dist.ht;
    assert(prox.ht >= dist.ht);
    m := BFAMR.add_by (prox.bf land dist.bf) (prox.ht -. dist.ht) !m
  in
  let hbflim = hbflim_of_inda inda in
  let dist_bfim = make_distal_bfim hbflim t in
  let prox_bfim = make_proximal_bfim dist_bfim t in
  let process_edge id = 
(* given the current proximal bf and a list of hbfs, recur, add the current snip,
 * then return the distal for things below and the height of things below. *)
    let rec aux curr_prox_bf = function
      | [] -> {bf=IntMap.find id dist_bfim; ht=0.}
      | x::l ->
          let with_us = curr_prox_bf lor x.bf in
          let curr_dist = aux with_us l 
          and curr_prox = {bf=curr_prox_bf; ht=x.ht}
          in
          Printf.printf "%d\t" id;
          add_snip ~prox:curr_prox ~dist:curr_dist;
          {curr_prox with bf=with_us} (* the distal in the calling recursion *)
    in
    let start_prox = {bf=IntMap.find id prox_bfim; ht=(Gtree.get_bl t id)} in
    let final_dist =
      aux
        start_prox.bf
        (List.sort ncompare_by_height 
          (try IntMap.find id hbflim with | Not_found -> []))
    in
    Printf.printf "%d\t" id;
    add_snip ~prox:start_prox ~dist:final_dist
  in
  try
    List.iter process_edge (Gtree.nonroot_node_ids t);
    !m
  with 
  | Not_found -> assert(false)

  (*
(* later have a lazy data cache with the induceds? *)
let pd_of_pr criterion pr = 
  pd_of_induced (Placerun.get_ref_tree pr) 
                (Induced.induced_of_placerun criterion pr)


*)
