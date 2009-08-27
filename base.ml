(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * some basic functions
*)

open MapsSets

let date_time_str () = 
  let the_time = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d/%02d/%d %02d:%02d:%02d" 
    (the_time.Unix.tm_mon+1) 
    the_time.Unix.tm_mday 
    (the_time.Unix.tm_year+1900)
    the_time.Unix.tm_hour 
    the_time.Unix.tm_min
    the_time.Unix.tm_sec



let rec list_fold_left3 f accu l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> accu
  | (a1::l1, a2::l2, a3::l3) -> 
      list_fold_left3 f (f accu a1 a2 a3) l1 l2 l3
  | (_, _, _) -> invalid_arg "list_fold_left3"



let triple_dot x y z size = 
  let tot = ref 0. in
  for i=0 to size - 1 do
    tot := !tot +. x.{i} *. y.{i} *. z.{i}
  done;
  !tot

let quad_dot x y z w size = 
  let tot = ref 0. in
  for i=0 to size - 1 do
    tot := !tot +. x.{i} *. y.{i} *. z.{i} *. w.{i}
  done;
  !tot

let pairwise_prod dest x y size =
  for i=0 to size - 1 do
    dest.{i} <- x.{i} *. y.{i}
  done

(* get the unique items from a list
 * slow, clearly.
 *)
let list_uniques linit = 
  List.rev (
    List.fold_left (
      fun l x ->
        if List.mem x l then l 
        else x :: l
    ) [] linit )


(* calculate the size of arrays in the list, checking that all arrays are of
 * the same size. *)
let size_of_array_list = function
  | x :: l ->
      let len = Gsl_vector.length x in
      List.iter (fun y -> assert(len = Gsl_vector.length y)) l;
      len
  | [] -> assert(false)
    

let combine_over_intmaps combine_fun keys m1 m2 = 
  try 
    List.fold_right 
      (fun key -> IntMap.add key 
                    (combine_fun (IntMap.find key m1) (IntMap.find key m2)))
      keys
      IntMap.empty
  with
  | Not_found -> invalid_arg "combine_over_maps: key not contained in map!"


(* 'a MapsSets.IntMap.t list -> 'a list MapsSets.IntMap.t = <fun>
 * combine all the maps into a single one, with k bound to a list of all of the
 * bindings for k in map_list.
 *)
let combine_intmaps_listly map_list = 
  List.fold_right
    (fun m ->
      IntMap.fold
        (fun k v sofar ->
          if IntMap.mem k sofar then 
            IntMap.add k (v::(IntMap.find k sofar)) sofar
          else
            IntMap.add k [v] sofar)
        m)
    (List.rev map_list) (* the above rev's things *)
    IntMap.empty

(* uniformly shuffle the elements of an array using the Knuth shuffle
 * http://en.wikipedia.org/wiki/Random_permutation
 * http://rosettacode.org/wiki/Knuth_shuffle#OCaml
 *)
let shuffle a = 
  let swap i j = let x = a.(i) in a.(i) <- a.(j); a.(j) <- x in
  for i = Array.length a - 1 downto 1 do 
    swap i (Random.int (i+1)) 
  done

(* make an integer permutation *)
let perm n = 
  let a = Array.init n (fun i -> i) in
  shuffle a;
  a

let complete_fold_left f = function
 | hd::tl -> List.fold_left f hd tl
 | [] -> invalid_arg "complete_fold_left: given empty list!"

(* find the i where the given x is geq a.(i) and leq a.(i+1)
# arr_between_spots [|0;1;3;6;7;7;7;7;8;9;|] 2;;
- : int list = [1]
# arr_between_spots [|0;1;3;6;7;7;7;7;8;9;|] 6;;
- : int list = [2; 3]
# arr_between_spots [|0;1;3;6;7;7;7;7;8;9;|] 7;;
- : int list = [3; 4; 5; 6; 7]
*)
let arr_between_spots a x = 
  let len = Array.length a in
  let betw = ref [] in
  for i = len-2 downto 0 do
    if a.(i) <= x && x <= a.(i+1) then
      betw := i::(!betw)
  done;
  !betw

let arr_assert_increasing a = 
  for i=0 to (Array.length a)-2 do
    assert(a.(i) <= a.(i+1));
  done

let int_div x y = (float_of_int x) /. (float_of_int y) 

(* a must be sorted increasing *)
let arr_pvalue a x = 
  arr_assert_increasing a;
  let alen = Array.length a in
  match arr_between_spots a x with
  | [] ->
      if x < a.(0) then 0.
      else if x > a.(alen-1) then 1.
      else assert(false)
  | l ->
      int_div
        (List.fold_left ( + ) 0 l)
        ((List.length l) * alen)



(* mask_to_list:
 * Mask an array into a list
 *)
let mask_to_list mask_arr a = 
  assert(Array.length mask_arr = Array.length a);
  let masked = ref [] in
(* count down so that we don't have to reverse after adding them *)
  for i = Array.length mask_arr - 1 downto 0 do
    if mask_arr.(i) then masked := a.(i)::!masked
  done;
  !masked


(* normalized_prob :
 * ll_list is a list of log likelihoods. this function gives the normalized
 * probabilities, i.e. exponentiate then our_like / (sum other_likes) 
 * *)
let normalized_prob ll_list = 
  List.map (
    fun log_like ->
      1. /. (
        List.fold_left ( +. ) 0. (
          List.map (
            fun other_ll -> exp (other_ll -. log_like)
          ) ll_list ) )
  ) ll_list


let time_fun f = 
  let prev = Sys.time () in
  f ();
  ((Sys.time ()) -. prev)

let print_time_fun name f = 
  Printf.printf "%s took %g seconds\n" name (time_fun f)
