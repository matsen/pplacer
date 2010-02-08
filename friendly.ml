(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * This code goes down an alignment and for every entry i looks for a j < i such
 * that i and j are as similar as possible. it records if i and j are identical.
 *
 * If two sequences have more than basic_max_mismatches mismatches, then they
 * are considered to be unrelated.
*)

let basic_max_mismatches = 5

type similarity = Unrelated | Similar of int * int 
type friend = Friendless | Identical of int | Friend of int

(* we want unrelated to be bigger (i.e. less similar) than any Similar one *)
let compare x y = 
  match (x, y) with
  | (Unrelated, Unrelated) -> 0
  | (Similar _, Unrelated) -> -1
  | (Unrelated, Similar _) -> 1
  | (a,b) -> Pervasives.compare a b

let is_gap c = 
  c = '?' || c = '-' 

(* report the similarity between two strings as either Unrelated (more than
 * max_mismatches mismatches) or Similar(n_mismatches, n_matches_to_gap) *)
let similarity max_mismatches s1 s2 = 
  let l = String.length s1 in
  assert(l = String.length s2);
  let mismatches = ref 0 
  and matches_to_gap = ref 0
  in
  try
    for i=0 to l-1 do
      let c1 = String.unsafe_get s1 i
      and c2 = String.unsafe_get s2 i in
      if c1 <> c2 then begin
        if is_gap c1 || is_gap c2 then
          incr matches_to_gap
        else begin
          incr mismatches;
          if !mismatches > max_mismatches then
            raise Exit
        end
      end
    done;
    Similar(!mismatches, !matches_to_gap)
  with
  | Exit -> Unrelated

(* match each entry in the alignment with the sequence with which it is most
 * similar. if no sequences are considered similar, than give None *)
let prev_pairing_of_alignment a = 
  let len = Array.length a in
  let pairing = Array.make len Friendless 
  and max_mismatches = ref basic_max_mismatches 
  in
  for i=1 to len-1 do
    let best_sim = ref Unrelated in
    try
      for j=0 to i-1 do
        max_mismatches := basic_max_mismatches;
        let our_sim = 
          similarity (!max_mismatches)
            (Alignment.get_seq a i) (Alignment.get_seq a j) in
        if 0 > compare our_sim (!best_sim) then begin
          (* we are more similar than the previous most similar seq *)
          best_sim := our_sim;
          match our_sim with
          | Unrelated -> assert(false)
          | Similar(0,0) -> begin
              pairing.(i) <- Identical j;
              raise Exit
          end
          | Similar(n_mismatches, _) -> begin
  (* here we reduce the number of possible mismatches so that the subsequent
   * sequence comparisons have to do strictly better than has been done before.
   * that way we can cut out early of a comparison *)
              max_mismatches := n_mismatches-1;  
              pairing.(i) <- Friend j
          end
        end;
      done;
    with | Exit -> () (* found an identical seq *)
  done;
  pairing

(* print out the similarities between sequences which are assigned to be friends
 * as a check *)
let fprint_similarities ch a prof = 
  for i=0 to (Array.length a)-1 do
    Printf.fprintf ch "\n  %s\n" (Alignment.get_seq a i);
    match prof.(i) with
    | Friendless -> Printf.fprintf ch "x\n";
    | Friend j -> 
        Printf.fprintf ch "f %s\n" (Alignment.get_seq a j);
    | Identical j -> 
        Printf.fprintf ch "i %s\n" (Alignment.get_seq a j);
  done

