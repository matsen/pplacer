(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
*)

let () = Random.self_init ()

(* bootstrap a list, with an option to modify the elements of the list through f
 * # boot_list (fun i x -> i+100*x) [1;2;3;4;5;6];;
 * - : int list = [102; 101; 201; 301; 501; 601]
 *
 * We check that the number of elements in the list is less than max_int, which is
 * 4,611,686,018,427,387,903 on a 64 bit machine.
 *)
let boot_list f l = 
  let n = List.length l in
  assert(n < max_int);
  let counts = Array.make n 0 in
  for i=1 to n do
    let draw = Random.int n in
    counts.(draw) <- counts.(draw) + 1
  done;
  let rec appendk acc elt k = 
    if k>0 then appendk ((f k elt)::acc) elt (k-1)
    else acc
  in
  let rec aux i acc = function
    | hd::tl -> aux (i+1) (appendk acc hd counts.(i)) tl
    | [] -> acc
  in
  let out = aux 0 [] l in
  assert(n = List.length out);
  List.rev out


let boot_placerun pr bootnum = 
  {
    pr with
    Placerun.name = (Placerun.get_name pr)^"_boot_"^(string_of_int bootnum);
    Placerun.pqueries = 
      boot_list
        (fun i pq -> 
          { pq with Pquery.name = (Pquery.name pq)^"_boot_"^(string_of_int i) })
        (Placerun.get_pqueries pr);
  }

