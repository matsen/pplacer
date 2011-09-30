(*
* Inner loop:
*  - find minimally distant pair of samples i and j
*  - merge them to make k, and normalize
*  - remove those pairs from the distance map, then add
*  - calcualate distances between i and k and j and k
*  - recalculate distances
*
* The main data structures are
*  - barkm: the bark map for the clustering tree
*  - bmapr: map from blobs to the trees representing their clustering sequence
*  - cset: set of clusterables, playing the part of a distance matrix (see below)
*  - blobim: records the blobs and their indices, for export
*)

open Ppatteries

module type BLOB =
sig
  type t
  val merge: t -> t -> t
  val compare: t -> t -> int
end

module Squash (B: BLOB) =
  struct

    module OrderedBlob =
      struct
        type t = B.t
        let compare = B.compare
      end

    module BMap = Map.Make (OrderedBlob)

    (* cble is short for clusterable. Such an object is maintained for every
     * pair of samples. *)
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

    (* Note: be completely sure that we sort by dist first *)
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

    (* This is where we keep our clusterables, so that we can pluck off the
     * smallest one each round. *)
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

    (* Note that the blobls can be non normalized as we call normf on them from
     * the beginning and pass those on to distf. *)
    let of_named_blobl given_distf normf named_blobl =
      let (n_blobs, start_bmap, start_blobim, start_barkm) =
        List.fold_left
          (fun (counter, bmap, blobim, barkm) (name, b) ->
            (counter+1,
              BMap.add b (Stree.leaf counter) bmap,
              IntMap.add counter b blobim,
              Newick_bark.map_set_name counter name barkm))
          (0, BMap.empty, IntMap.empty, IntMap.empty)
          named_blobl
      in
      assert (n_blobs > 0);
      (* we store our normf results in normm *)
      let start_normm = BMap.mapi (fun b _ -> normf b) start_bmap in
      let distf normm b b' =
        given_distf ~x1:(BMap.find b normm) ~x2:(BMap.find b' normm) b b'
      in
      (* Build up our set of clusterables. *)
      Printf.printf "Preparing the objects to be clustered...";
      flush_all ();
      let rec init_aux accu = function
        | b::l ->
            init_aux
              (List.fold_left
                (fun accu' b' ->
                  CSet.add (cble_of_blobs (distf start_normm) b b') accu')
                accu
                l)
              l
        | [] -> accu
      in
      let start_cset = init_aux CSet.empty (List.map snd named_blobl) in
      print_endline "done.";
      (* now actually perform the clustering *)
      (* * Main recursion ** *)
      let rec merge_aux bmap cset free_index normm blobim barkm =
        Printf.printf "step %d of %d\n" (free_index - n_blobs + 1) n_blobs;
        flush_all ();
        if CSet.cardinal cset = 0 then begin
          let (_, stree) = get_only_binding bmap in
          (Gtree.gtree stree barkm, blobim)
        end
        else begin
          let next = CSet.min_elt cset in
          let tsmall = BMap.find next.small bmap
          and tbig = BMap.find next.big bmap
          and merged = B.merge next.small next.big
          in
          let new_normm = BMap.add merged (normf merged) normm in
          (* Calculate the proper branch length for the blob to the merged, then
           * add it to the given bark map. *)
          let add_bl b barkm =
            Newick_bark.map_set_bl
              (Stree.top_id (BMap.find b bmap))
              (distf new_normm b merged)
              barkm
          in
          merge_aux
            (BMap.add
              merged
              (Stree.node free_index [tsmall; tbig])
              (BMap.remove next.small (BMap.remove next.big bmap)))
            (cset_map (perhaps_replace_blob (distf new_normm) ~oldb:(next.small) ~newb:merged)
              (cset_map (perhaps_replace_blob (distf new_normm) ~oldb:(next.big) ~newb:merged)
                (CSet.remove next cset)))
            (free_index+1)
            new_normm
            (IntMap.add free_index merged blobim)
            (Newick_bark.map_set_name
              free_index
              (string_of_int free_index)
              (add_bl next.small (add_bl next.big barkm)))
        end
      in
      merge_aux
        start_bmap start_cset n_blobs start_normm start_blobim start_barkm

    (* mimic clusters blobl with the same steps as in the supplied tree, and
     * spit out a blobim which represents what would have resulted had we done
     * the clustering in that way. *)
    let mimic t blobl =
      let blobim = ref IntMap.empty in
      let set_blob i b = blobim := IntMap.check_add i b (!blobim) in
      List.iteri set_blob blobl;
      let _ =
        Gtree.recur
          (fun i -> function
            | [b1; b2] -> let m = B.merge b1 b2 in set_blob i m; m
            | _ -> invalid_arg "mimic: tree isn't strictly bifurcating")
          (fun i -> IntMap.find i (!blobim))
          t
      in
      !blobim
  end


