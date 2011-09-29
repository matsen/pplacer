open Ppatteries
open Ptree

(* we use the IdblSet to choose what is the next best edge choice *)

type idbl = {id : int; bl : float}

(* we only want Pends in the set! *)
module OrderedIdbl = struct
  type t = idbl
  (* order first by bl, then by id *)
  let compare a b =
    match compare a.bl b.bl with 0 -> compare a.id b.id | x -> x
end

module IdblSet = Set.Make(OrderedIdbl)

let idblset_of_ptree exclude_ids pt =
  fold
    (fun id e s ->
      if IntSet.mem id exclude_ids then s
      else match e with
      | Pend(id',bl,_) -> assert(id=id'); IdblSet.add {id=id;bl=bl} s
      | Inte(_,_,_) -> s)
    pt
    IdblSet.empty

let hashtbl_freplace f h id = strict_replace h id (f (find h id))


(* <sound of rubber hitting road> *)
(* we delete the specified pendant branch, extend the other branches to maintain
 * the structure of the tree, and perform the corresponding
 * modification to idbls (which gets returned). *)
let delete_pend pt idbl idbls =
  match find pt idbl.id with
  | Inte(_,_,_) -> failwith "can't delete internal edge"
  | Pend(_, _, eidl) ->
      let del_idbls = IdblSet.remove idbl idbls in
      strict_remove pt idbl.id;
      (match eidl with
      | [] | [_] -> assert false
      | [eid1; eid2] -> begin
        (* degree two-- heal the wound. *)
        match ((eid1,find pt eid1), (eid2,find pt eid2)) with
        | ((_,Pend(_,_,_)),(_,Pend(_,_,_))) ->
            raise (Not_implemented "can't cut down trees without internal edges")
        | ((id1, Inte(bl1,l1,r1)), (id2, Inte(bl2,l2,r2))) ->
        (* we are deleting a pendant edge which touches two internal edges.
         * we join these two internal edges together. *)
        (* id2_far_side is the set of edges far from idbl.id *)
            let id2_far_side = get_other_side [idbl.id; id1] l2 r2 in
         (* we need to re-attach the edges which point to id2 *)
            List.iter (tree_update ~src:id2 ~dst:id1 pt) id2_far_side;
            strict_replace pt id1
              (Inte(bl1+.bl2,
                get_other_side [idbl.id; id2] l1 r1,
                id2_far_side));
            strict_remove pt id2;
            del_idbls
        | ((pid, Pend(orig_id,pbl,pl)), (iid, Inte(ibl,il,ir)))
        | ((iid, Inte(ibl,il,ir)), (pid, Pend(orig_id,pbl,pl))) ->
        (* we are deleting one edge of a cherry. in this case, we delete the
         * other pendant edge and extend the just-proximal edge with the pendant
         * edge's length. That way, we don't have to update the other edges with
         * a different id. *)
            assert(sorted_list_eq pl [iid; idbl.id]);
            strict_replace pt iid
              (Pend(orig_id, pbl+.ibl, get_other_side [idbl.id; pid] il ir));
            strict_remove pt pid;
            IdblSet.add
              {id=iid; bl=pbl+.ibl}
              (IdblSet.remove {id=pid; bl=pbl} del_idbls)
      end
      | eidl ->
        (* degree greater than two:
        * delete idbl.id from the edge lists of the other nodes *)
          let check_del1 l =
            let out = List.filter ((<>) idbl.id) l in
        (* make sure that internal nodes still have degree greater than two *)
            assert(1 < List.length out);
            out
          in
          List.iter
            (hashtbl_freplace
              (function
                | Pend(id,bl,l) -> Pend(id, bl, check_del1 l)
                | Inte(bl,l,r) -> Inte(bl, check_del1 l, check_del1 r))
              pt)
            eidl;
          del_idbls
      )

type stop_criterion =
  | Branch_length of float
  | Leaf_count of int

let until_stopping safe exclude_ids criterion pt =
  let should_stop = match criterion with
    | Branch_length bl -> fun s -> (IdblSet.min_elt s).bl > bl
    | Leaf_count c -> fun s -> (IdblSet.cardinal s) <= c
  in
  let rec aux accu s =
    if s = IdblSet.empty then accu
    else begin
      let m = IdblSet.min_elt s in
      if should_stop s then accu
      else match find pt m.id with
      | Pend(orig_id, bl, _) ->
          assert(bl = m.bl);
          let new_s = delete_pend pt m s in
          if safe then check pt;
          aux ((orig_id,m.bl,to_stree pt)::accu) new_s
      | Inte(_,_,_) -> assert false
    end
  in
  List.rev (aux [] (idblset_of_ptree exclude_ids pt))
