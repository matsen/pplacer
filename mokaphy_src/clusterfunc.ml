(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.

* goal: greedy algorithm which minimizes the distance between sibling pairs in the tree
* we calculate all pairwise distances
* inner loop
  * find minimally distant pair of samples i and j by folding over map
  * coalesce them to make k, and normalize
  * remove those pairs from the distance map, then add
  * calcualate distances between i and k and j and k
  * coalesce in the tree map
    * note that we will have to keep track of the next available index
  * recalculate distances

*)

open MapsSets

module type BLOB = 
sig 
  type t 
  type tree
  val compare: t -> t -> int
  val distf: tree -> t -> t -> float
  val to_string: tree -> string
  val merge: t -> t -> t
  val hook: t -> unit
end

module Cluster (B: BLOB) =
  struct

    module OrderedBlob = 
      struct
        type t = B.t
        let compare = B.compare
      end

    module BMap = Map.Make (OrderedBlob)

    (* cble is short for clusterable *)
    type cble = 
      {
        dist : float;
        small : B.t;
        big : B.t;
      }

    let cble_of_blobs rt b b' = 
      let (small, big) = if compare b b' < 0 then (b, b') else (b', b) in
      { dist = B.distf rt b b'; small = small; big = big; }

    (* be completely sure that we sort by dist first *)
    let compare_cble a b =
      let cdist = compare a.dist b.dist in
      if cdist <> 0 then cdist
      else Pervasives.compare a b

    module OrderedCble = 
      struct
        type t = cble
        let compare = compare_cble
      end

    module CSet = Set.Make (OrderedCble)

    let ingreds_of_named_blobl rt blobl = 
      let counter = ref 0 
      and barkm = ref IntMap.empty
      and bmap = ref BMap.empty
      and cset = ref CSet.empty
      in 
      (* let set_bl id bl = barkm := Newick_bark.map_set_bl id bl (!barkm) *)
      let set_name id name = barkm := Newick_bark.map_set_name id name (!barkm)
      in
      print_endline "making the leaves";
      List.iter
        (fun (name, b) ->
          set_name (!counter) name;
          bmap := BMap.add b (Stree.leaf (!counter)) (!bmap);
          incr counter;
        )
        blobl;
      let rec aux = function 
        | (_, b)::l -> 
            List.iter 
              (fun (_, b') -> cset := CSet.add (cble_of_blobs rt b b') (!cset))
              l;
            aux l
        | [] -> ()
      in
      Printf.printf "making the cble set...";
      aux blobl;
      print_endline "done.";
      (!bmap, !cset)
  
  end

module PreBlob = 
  struct
    type t = Mass_map.Pre.t
    type tree = Newick.t
    let compare = Pervasives.compare
    let distf = Kr_distance.dist_of_pres 1.
    let to_string = Newick.to_string
    let merge = ( @ )
    let hook _ = ()
  end

module PreCluster = Cluster (PreBlob)

let t_named_prel_of_prl prl = 
  (Cmds_common.list_get_same_tree prl,
  List.map
    (fun pr -> 
      Printf.printf "reading %s\n" (Placerun.get_name pr);
      (Placerun.get_name pr,
      Mass_map.Pre.of_placerun Mass_map.Unweighted Placement.ml_ratio pr))
    prl)

