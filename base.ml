(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * some basic functions
*)

open MapsSets

let round x = int_of_float (floor (x +. 0.5))

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

(* funny. 
# list_sub 4 [1;2;3;4;5;6;7;8];;
- : int list = [1; 2; 3; 4]
# list_sub ~start:2 ~len:4 [1;2;3;4;5;6;7;8];;
- : int list = [3; 4; 5; 6]
 * *)
let list_sub ?start:(start=0) ~len l = 
  Array.to_list (Array.sub (Array.of_list l) start len)


exception Different of int

let raise_if_different cmp x1 x2 = 
  let c = cmp x1 x2 in
  if c <> 0 then raise (Different c)

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

(*
# list_find_loc 2 [0;1;2;3;4;5;6;7];;
- : int = 2
*)
let list_find_loc x l = 
  let rec aux i = function
    | hd::tl -> if x = hd then i else aux (i+1) tl
    | [] -> raise Not_found
  in 
  aux 0 l


(* given f and a list, apply f to all unordered pairs of the list.
# list_iterpairs (Printf.printf "(%d,%d)\t") [1;2;3];;
(1,2)   (1,3)   (2,3)   - : unit = ()
 * *)
let rec list_iterpairs f = function
  | x::l -> List.iter (f x) l; list_iterpairs f l
  | [] -> ()

(* apply f to each pair in each pairing of the lists
# list_list_iterpairs (Printf.printf "(%d,%d)\t") [[1;2];[3];[4;5]];;
(1,3)   (2,3)   (1,4)   (1,5)   (2,4)   (2,5)   (3,4)   (3,5)   - : unit = ()
* note that we don't get (1,2).
*)
let list_list_iterpairs f ll = 
  list_iterpairs
    (fun l1 l2 ->
      List.iter 
        (fun x -> List.iter (fun y -> f x y) l2)
        l1)
    ll

(* get from map, but return an empty list if not in map *)
let get_from_list_intmap id m = 
  if IntMap.mem id m then IntMap.find id m
  else []

(*
# let divbyk k x = x mod k = 0;;                                   
val divbyk : int -> int -> bool = <fun>
# let x = find_multiple_matches [divbyk 2; divbyk 3] [1;2;3;4;5;6;7;8];;
val x : int list = [6]
*)
let find_multiple_matches f_list =
  let rec aux accu = function
    | [] -> List.rev accu
    | x::l ->
        if (List.fold_left 
             (fun accu f ->
               accu+(if f x then 1 else 0))
             0
             f_list)
           > 1 then
             aux (x::accu) l
        else
          aux accu l
  in 
  aux [] 


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

(* 
 * 'a list MapsSets.IntMap.t list -> 'a list MapsSets.IntMap.t = <fun>
 * combine all the maps into a single one, with k bound to the concatenated set
 * of bindings for k in map_list.
 *)
let combine_list_intmaps map_list = 
  IntMap.map
    List.flatten
    (combine_intmaps_listly map_list)


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


(* the L_1 norm of a float list *)
let normalized_prob fl = 
  let sum = List.fold_left ( +. ) 0. fl in
  List.map (fun x -> x /. sum) fl

(* the L_1 norm of a float array *)
let arr_normalized_prob fa = 
  let sum = Array.fold_left ( +. ) 0. fa in
  Array.map (fun x -> x /. sum) fa

(* ll_normalized_prob :
 * ll_list is a list of log likelihoods. this function gives the normalized
 * probabilities, i.e. exponentiate then our_like / (sum other_likes) 
 * have to do it this way to avoid underflow problems.
 * *)
let ll_normalized_prob ll_list = 
  List.map 
    (fun log_like ->
      1. /. 
        (List.fold_left ( +. ) 0. 
          (List.map 
            (fun other_ll -> exp (other_ll -. log_like)) 
            ll_list)))
    ll_list

let time_fun f = 
  let prev = Sys.time () in
  f ();
  ((Sys.time ()) -. prev)

let print_time_fun name f = 
  Printf.printf "%s took %g seconds\n" name (time_fun f)
