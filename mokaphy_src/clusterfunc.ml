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
  val normf: t -> float
  val to_string: tree -> string
  val merge: t -> t -> t
  val hook: tree -> t -> string -> unit
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

    let cble_of_blobs distf b b' = 
      let (small, big) = if compare b b' < 0 then (b, b') else (b', b) in
      { dist = distf b b'; small = small; big = big; }

    let blob_in_cble b c = b = c.small || b = c.big

    (* be completely sure that we sort by dist first *)
    let compare_cble a b =
      let cdist = compare a.dist b.dist in
      if cdist <> 0 then cdist
      else Pervasives.compare a b

    let replace_blob distf oldb newb c = 
      if c.small = oldb then cble_of_blobs distf c.big newb
      else if c.big = oldb then cble_of_blobs distf c.small newb
      else invalid_arg "replace_blob: blob not found"

    let perhaps_replace_blob distf ~oldb ~newb c = 
      if blob_in_cble oldb c then replace_blob distf oldb newb c
      else c

    module OrderedCble = 
      struct
        type t = cble
        let compare = compare_cble
      end

    module CSet = Set.Make (OrderedCble)

    let cset_map f s = CSet.fold (fun x -> CSet.add (f x)) s CSet.empty

    let ingreds_of_named_blobl rt blobl = 
      let counter = ref 0 
      and barkm = ref IntMap.empty
      and bmap = ref BMap.empty
      and cset = ref CSet.empty
      in 
      let set_name id name = barkm := Newick_bark.map_set_name id name (!barkm)
      in
      print_endline "making the leaves";
      List.iter
        (fun (name, b) ->
          set_name (!counter) name;
          bmap := BMap.add b (Stree.leaf (!counter)) (!bmap);
          B.hook rt b (Printf.sprintf "%d.tre" (!counter));
          incr counter;
        )
        blobl;
      let rec aux = function 
        | (_, b)::l -> 
            List.iter 
              (fun (_, b') -> 
                cset := CSet.add (cble_of_blobs (B.distf rt) b b') (!cset))
              l;
            aux l
        | [] -> ()
      in
      Printf.printf "making the cble set...";
      aux blobl;
      print_endline "done.";
      (!bmap, !cset, !barkm, !counter)


    (* BEGIN crazy work around until ocaml 3.12 *)
    exception First of B.t

    let first_key m = 
      try 
        BMap.iter (fun k _ -> raise (First k)) m; 
        invalid_arg "empty map given to first_key"
      with
      | First k -> k

    let get_only_binding m = 
      let k = first_key m in
      match BMap.remove k m with
      | m' when m' = BMap.empty -> (k, BMap.find k m)
      | _ -> invalid_arg "get_only_binding: more than one binding"
    (* END crazy work around until 3.12 *)
      
    let of_ingreds rt start_bmap start_cset start_barkm start_free_index = 
      let barkm = ref start_barkm
      and normm = ref (BMap.mapi (fun b _ -> 
        Printf.printf "%g\n" (B.normf b); 
        B.normf b) start_bmap)
      and n_blobs = BMap.fold (fun _ _ i -> i+1) start_bmap 0 
      in
      let distf b b' = 
        (BMap.find b !normm) *. (BMap.find b' !normm) *. (B.distf rt b b')
      in
      assert (n_blobs > 0);
      let rec aux bmap cset free_index = 
        Printf.printf "step %d of %d\n" (free_index - n_blobs) (n_blobs - 1);
        flush_all ();
        let set_bl_for b bl = 
          barkm := Newick_bark.map_set_bl 
                     (Stree.top_id (BMap.find b bmap)) bl (!barkm)
        in
        if CSet.cardinal cset = 0 then begin
          let (_, stree) = get_only_binding bmap in
          Gtree.gtree stree (!barkm)
        end
        else begin
          let next = CSet.min_elt cset in
          let tsmall = BMap.find next.small bmap
          and tbig = BMap.find next.big bmap
          and merged = B.merge next.small next.big
          in
          normm := BMap.add merged (B.normf merged) (!normm);
          B.hook rt merged (Printf.sprintf "%d.tre" free_index);
          set_bl_for next.small (distf next.small merged);
          set_bl_for next.big (distf next.big merged);
          aux 
            (BMap.add
              merged
              (Stree.node free_index [tsmall; tbig])
              (BMap.remove next.small (BMap.remove next.big bmap)))
            (cset_map (perhaps_replace_blob distf ~oldb:(next.small) ~newb:merged)
              (cset_map (perhaps_replace_blob distf ~oldb:(next.big) ~newb:merged)
                (CSet.remove next cset)))
            (free_index+1)
        end
      in
      aux start_bmap start_cset start_free_index

    let of_named_blobl rt blobl =
      let (start_bmap, start_cset, start_barkm, start_free_index) = 
        ingreds_of_named_blobl rt blobl
      in
      of_ingreds rt start_bmap start_cset start_barkm start_free_index

  end


(* NOTE: mass should be normalized when making PreBlobs! *)

module PreBlob = 
  struct
    type t = Mass_map.Pre.t
    type tree = Decor_gtree.t
    let compare = Pervasives.compare
    let distf = Kr_distance.dist_of_pres 1.
    let normf a = 1. /. (Mass_map.Pre.total_mass a)
    let to_string = Newick.to_string
    let merge b1 b2 = b1 @ b2
    let hook rt pre name = 
      let tot = Mass_map.Pre.total_mass pre in
      assert(tot > 0.);
      Placeviz_core.write_fat_tree
       400. (* mass width *)
       1.   (* log coeff *)
       name
       rt
       (Mass_map.By_edge.of_pre ~factor:(1. /. tot) pre)
  end

module PreCluster = Cluster (PreBlob)

