(* mokaphy v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries

let ppr_kr_info ff info_map = 
  IntMapFuns.ppr_gen
    (Ppr.ppr_list
      (fun ff (bl, v) -> 
        Format.fprintf ff 
          "(%g, %a)" 
          bl
          Ppr.ppr_float_array v))
    ff
    info_map

let ppr_pair_float_uptri ff fu = 
  Uptri.ppr_uptri 
    (fun ff (x,y) -> Format.fprintf ff "(%g, %g)" x y)
    ff 
    fu

let write_named_float_uptri ch names u =
  String_matrix.write_named_padded ch names
    (MatrixFuns.init (Uptri.get_dim u) (Uptri.get_dim u)
      (fun i j -> 
        if i < j then (Printf.sprintf "%g" (Uptri.get u i j))
        else ""))

(* add v2 to v1 (which is modified in place) *)
let v_addto v1 v2 = 
  for i=0 to (Array.length v1)-1 do
    v1.(i) <- v1.(i) +. v2.(i)
  done

(* multiply a vector times a scalar. functional. *)
let v_times_scalar v s =
  Array.map (( *. ) s) v

(* take the vector sum of a float array list. no side effects. *)
let v_list_sum = function
  | hd::tl ->
    let v = Array.copy hd in
    List.iter (v_addto v) tl;
    v
  | [] -> assert(false)


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

(* make a min to max array such that their differences in sequence are evenly
 * spaced *)
let logarithmically_evenly_spaced n_x min_x max_x = 
  let log_min_x = log min_x
  and log_max_x = log max_x in
  let delta = (log_max_x -. log_min_x) /. (float_of_int (n_x-1)) in
  Array.init n_x
    (fun i -> exp (log_min_x +. (float_of_int i) *. delta))

(* find the i where the given x is geq a.(i) and leq a.(i+1)
 * useful for dealing with ties
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
let arr_avg_pvalue a x = 
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

(* just calculate the fraction of elements of a which are geq x.
 * that's the probability that something of value x or greater was drawn from
 * the distribution of a.
 * clearly, l doesn't need to be sorted *)
let list_onesided_pvalue l x = 
  int_div
    (List.fold_left
      (fun accu a_elt ->
        if a_elt >= x then accu+1
        else accu)
      0 l)
    (List.length l)

let mean l = 
  (List.fold_left ( +. ) 0. l) /. (float_of_int (List.length l))

let sq x = x *. x

(*
# let data = [2.;4.;4.;4.;5.;5.;7.;9.];;
val data : float list = [2.; 4.; 4.; 4.; 5.; 5.; 7.; 9.]
# mean_std_dev(data);;
- : float * float = (5., 2.)
*)
let mean_std_dev l = 
  let tot = ref 0. in
  let mu = mean l in
  List.iter
    (fun x -> tot := !tot +. sq (x -. mu))
    l;
  (mu, sqrt(!tot /. (float_of_int (List.length l))))

let std_dev l = snd(mean_std_dev l)
