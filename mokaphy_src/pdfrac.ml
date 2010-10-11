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

(* dbf: distal bitfield *)
type dbf = 
  {
    distal : float;
    bf : Bitfield.t;
  }

(* the dbf list IntMap of samples on each edge *)
let dbflim_of_inda inda = 
  let m = ref IntMap.empty in 
  for i=0 to (Array.length inda)-1 do
    let ei = Bitfield.ei i in
    IntMap.iter
      (fun loc distal -> 
        m := IntMapFuns.add_listly loc {distal=distal; bf=ei} !m)
      inda.(i);
  done;
  !m

let union dbfl = 
  match List.map (fun dbf -> dbf.bf) dbfl with
  | [] -> Bitfield.empty
  | x::l -> List.fold_left (lor) x l

let find_listly i m = try IntMap.find i m with | Not_found -> []

let make_distal_bfim dbflim t = 
  let m = ref IntMap.empty in
  let add id bf = m := IntMapFuns.check_add id bf !m; bf in
  let bfget id = union (find_listly id dbflim) in
  let _ = 
    Gtree.recur
      (fun id below -> add id (List.fold_left (lor) (bfget id) below))
      (fun id -> add id (bfget id))
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


  (*
(* we put in a bfim from the previous step and get out a map from the various
 * color combinations to the amount of branch length corresponding to that color
 * combination *)
let make_fbfm t bfim = 
  let m = ref BFAMR.M.empty in
  let add_snip bf snip_len =
    assert(snip_len >= 0.);
    m := BFAMR.add_by bf snip_len !m;
  in
  let 
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
