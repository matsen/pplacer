(*
 * mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
*)

open MapsSets

let check_add x s =
  if StringSet.mem x s then invalid_arg "check_add"
  else StringSet.add x s

let disj_union s1 s2 =
  if StringSet.empty = StringSet.inter s1 s2 then 
    StringSet.union s1 s2
  else
    invalid_arg "disj_union"

let list_disj_union = List.fold_left disj_union StringSet.empty

type isect = 
  {
    size : int;
    factor : float;
    inter : StringSet.t;
    s1 : StringSet.t;
    s2 : StringSet.t;
    v1 : int;
    v2 : int;
  }

let make_factor i a b = 
  (float_of_int i) /. (sqrt (float_of_int (a*b)))

let build_isect s1 s2 v1 v2 = 
  let inter = StringSet.inter s1 s2 in
  let size = StringSet.cardinal inter in
  {
    size = size;
    factor = make_factor size (StringSet.cardinal s1) (StringSet.cardinal s2);
    inter = inter;
    s1 = s1;
    s2 = s2;
    v1 = v1;
    v2 = v2;
  } 

let size_first is = [float_of_int is.size; is.factor]
let factor_first is = [is.factor; float_of_int is.size]

let two_tier_compare via a b = 
  let first = compare (via a) (via b) in
  if first <> 0 then first
  else compare a b

module OrderedIsect = struct
  type t = isect
  let compare = two_tier_compare size_first
end


(*
 * First construct all subsets, and order them according to compare.
 * Do the following recursively, given a set :
   * Take the biggest one and save it to our list.
   * Then look for any overlap of that and another
*)

module IsectSet = Set.Make(OrderedIsect)

(* 
 * map from boot value to sets below.
 * note that we only store non-singletons.
 * *)
let ssim_of_tree t = 
  let m = ref IntMap.empty in
  let my_add k v = m := IntMap.add k v !m in
  let rec aux = function
    | Stree.Node(id, tL) ->
        let below = list_disj_union (List.map aux tL) in
        my_add (int_of_float (Gtree.get_boot t id)) below;
        below
    | Stree.Leaf(id) ->
        StringSet.singleton (Gtree.get_name t id)
  in
  let _ = aux (Gtree.get_stree t) in
  !m

let build_isects m1 m2 = 
  IntMap.fold
    (fun v1 s1 iss ->
      IntMap.fold
        (fun v2 s2 -> IsectSet.add (build_isect s1 s2 v1 v2))
        m2
        iss)
    m1
    IsectSet.empty

let overlap is1 is2 = 
  (is1.inter = is2.inter) || (is1.s1 = is2.s1) || (is1.s2 = is2.s2) 

let no_overlap is1 is2 = not (overlap is1 is2)

let pick iss = 
  let top = IsectSet.max_elt iss in
  (top, IsectSet.filter (no_overlap top) iss)

let pickn start_iss n = 
  let rec aux acc i iss = 
    if i=0 then List.rev acc 
    else begin
      let (top, rest) = pick iss in
      aux (top::acc) (i-1) rest
    end
  in
  aux [] n start_iss

let pick_all start_iss = 
  let rec aux acc iss = 
    if iss = IsectSet.empty then List.rev acc 
    else begin
        let (top, rest) = pick iss in
        aux (top::acc) rest
    end
  in
  aux [] start_iss

let write_isect_contents ch is = 
  StringSet.iter
    (fun s -> Printf.fprintf ch "%s\t%d\t%d\n" s is.v1 is.v2)
    is.inter

let isect_contents_to_file fname isl = 
  let ch = open_out fname in
  Printf.fprintf ch "sample_id\tphy\ttax\n";
  List.iter (write_isect_contents ch) isl;
  close_out ch

let write_isect_stats ch is = 
  Printf.fprintf ch "%d\t%d\t%d\t%g\n" is.v1 is.v2 is.size is.factor

let isect_stats_to_file fname isl = 
  let ch = open_out fname in
  Printf.fprintf ch "phy\ttax\tsize\tfactor\n";
  List.iter (write_isect_stats ch) isl;
  close_out ch

let filter_by_factor cutoff isectl = 
  List.filter (fun isect -> isect.factor > cutoff) isectl

let seecluster prefix cutoff treefname1 treefname2 = 
  let t1 = Newick.of_file treefname1
  and t2 = Newick.of_file treefname2
  in
  let ssim1 = ssim_of_tree t1
  and ssim2 = ssim_of_tree t2
  in
  let iss = build_isects ssim1 ssim2 in
  let alll = pick_all iss in
  let filtered = filter_by_factor cutoff alll in
  isect_contents_to_file (prefix^".contents.tab") filtered;
  isect_stats_to_file (prefix^".stats.tab") filtered;
  ()
