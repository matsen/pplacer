(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.

* goal: greedy algorithm which minimizes the distance between sibling pairs in the tree
* first we make pres from every file
  * they can be tax distances or not
* then we make a distance function
  * Kr_distance.dist_of_pres
* we calculate all pairwise distances
* inner loop
  * have a counter for indices and a map for the bark
  * find minimally distant pair of samples i and j by folding over map
  * coalesce them to make k, and normalize
  * remove that pair from the distance map, then add
  * calcualate distances between i and k and j and k
  * coalesce in the tree map
    * note that we will have to keep track of the next available index
  * recalculate distances

To store the distances, we can have a map which goes from sorted pairs of pres to distances
Have a function which finds the min value
need a map from pres to trees


Functorial way: supply
  a type of thing
  a comparison function for the things
  a distance between the things
  a way to merge the things
  a hook to apply to the merged things
*)

open MapsSets


module type THING = 
sig 
  type t 
  val compare: t -> t -> int
  val distance: t -> t -> float
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

  end

module PreThing = 
  struct
    type t = Mass_map.Pre.t
    let compare = Pervasives.compare
    let distance = Kr_distance.dist 


module FloatMapFuns = MapFuns (OrderedFloat) (StringableFloat)
