(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.

* inner loop
  * find minimally distant pair of samples i and j by folding over map
  * merge them to make k, and normalize
  * remove those pairs from the distance map, then add
  * calcualate distances between i and k and j and k
  * recalculate distances
*)

open MapsSets
open Fam_batteries

module type BLOB = 
sig 
  type t 
  val merge: t -> t -> t
  val compare: t -> t -> int
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

    (* we require distf to be passed in here, so that we can perform some normm
     * fun as below *)
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

    (* note that the blobls can be non normalized as we call normf on them from
     * the beginning and pass those on to distf *)
    let of_named_blobl given_distf normf blobl = 
      let counter = ref 0 
      and barkm = ref IntMap.empty
      and bmapr = ref BMap.empty
      and csetr = ref CSet.empty
      and blobim = ref IntMap.empty
      in 
      let set_name id name = barkm := Newick_bark.map_set_name id name (!barkm)
      in
      List.iter
        (fun (name, b) ->
          set_name (!counter) name;
          bmapr := BMap.add b (Stree.leaf (!counter)) (!bmapr);
          blobim := IntMap.add (!counter) b (!blobim);
          incr counter;
        )
        blobl;
      (* we store our normf results in normm *)
      let normm = ref (BMap.mapi (fun b _ -> normf b) (!bmapr)) in
      let distf b b' = 
        given_distf ~x1:(BMap.find b !normm) ~x2:(BMap.find b' !normm) b b'
      in
      let rec init_aux = function 
        | b::l -> 
            List.iter 
              (fun b' -> csetr := CSet.add (cble_of_blobs distf b b') (!csetr))
              l;
              init_aux l
        | [] -> ()
      in
      Printf.printf "making the cble set...";
      flush_all ();
      init_aux (List.map snd blobl);
      print_endline "done.";
      (* now actually perform the clustering *)
      let n_blobs = BMap.fold (fun _ _ i -> i+1) (!bmapr) 0 in
      assert (n_blobs > 0);
      let rec merge_aux bmap cset free_index = 
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
          normm := BMap.add merged (normf merged) (!normm);
          blobim := IntMap.add free_index merged (!blobim);
          set_bl_for next.small (distf next.small merged);
          set_bl_for next.big (distf next.big merged);
          barkm := 
            Newick_bark.map_set_name free_index (string_of_int free_index) (!barkm);
          merge_aux 
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
      let t = merge_aux (!bmapr) (!csetr) (!counter) in
      (t, !blobim)

    (* mimic clusters blobl with the same steps as in the supplied tree, and
     * spit out a blobim which represents what would have resulted had we done
     * the clustering in that way. *)
    let mimic t blobl = 
      let blobim = ref IntMap.empty in 
      let set_blob i b = blobim := IntMapFuns.check_add i b (!blobim) in
      ListFuns.iteri set_blob blobl;
      let _ = 
        Gtree.recur
          (fun i -> function
            | [b1; b2] -> let m = B.merge b1 b2 in set_blob i m; m
            | _ -> assert false)
          (fun i -> IntMap.find i (!blobim))
          t
      in
      !blobim
  end


module PreBlob = 
  struct
    type t = Mass_map.Pre.t
    let compare = Pervasives.compare
    let merge b1 b2 = b1 @ b2
  end

module PreCluster = Cluster (PreBlob)

