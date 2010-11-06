(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.

* goal: greedy algorithm which minimizes the distance between sibling pairs in the tree
* we calculate all pairwise distances
* inner loop
  * have a counter for indices and a map for the bark
  * find minimally distant pair of samples i and j by folding over map
  * coalesce them to make k, and normalize
  * remove those pairs from the distance map, then add
  * calcualate distances between i and k and j and k
  * coalesce in the tree map
    * note that we will have to keep track of the next available index
  * recalculate distances

*)

open MapsSets

module type THING = 
sig 
  type t 
  type tree
  val compare: t -> t -> int
  val distance: tree -> t -> t -> float
  val to_string: tree -> string
  val merge: t -> t -> t
  val hook: t -> unit
end

module Cluster (T: THING) =
  struct
    type thingpair = T.t * T.t

    module OrderedThingPair = struct
      type t = thingpair
      let compare = Pervasives.compare
    end

    module TPM = Map.Make(OrderedThingPair)

    let pdistance (v1, v2) = T.distance v1 v2

    (* a crazy work around until 3.12 *)
    exception First of thingpair
    let first_key m = 
      try 
        TPM.iter (fun k _ -> raise (First k)) m; 
        invalid_arg "empty list given to first_key"
      with
      | First k -> k

  (* Given a TPM, we find the key with the smallest value *)
    let min_key_by_val m = 
      let firstk = first_key m in
      let min_val = ref (TPM.find firstk m) in
      TPM.fold 
        (fun k v best ->
          if !min_val > v then begin min_val := v; k end
          else best)
        m
        firstk

  end

module PreThing = 
  struct
    type t = Mass_map.Pre.t
    type tree = Newick.t
    let compare = Pervasives.compare
    let distance = Kr_distance.dist_of_pres 1.
    let to_string = Newick.to_string
    let merge = ( @ )
    let hook _ = ()
  end

module PreCluster = Cluster (PreThing)
