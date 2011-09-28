exception Other_side of int list * int list * int list
exception Side_without of int * int list * int list
exception Not_implemented of string
exception Missing_edge of int * string
exception Pend_mismatch of int list (*desired*) * int list (*found*)
exception Inte_mismatch of int list (*desired*) * int list (*foundl*) * int list (*foundr*)

open Ppatteries

type edge =
  (*        id    bl      connections *)
  | Pend of int * float * int list
  | Inte of       float * int list * int list

(* oh yes! a mutable data structure, just for fun. *)
type ptree = (int, edge) Hashtbl.t


(* *** EDGE UTILS *** *)

let sorted_list_eq l1 l2 = List.sort l1 = List.sort l2

(* assert that one_side is a (potentially resorted version of) l or r, and
 * return the one that it is not *)
let get_other_side one_side l r =
  if sorted_list_eq one_side l then r
  else if sorted_list_eq one_side r then l
  else raise (Other_side (one_side, l, r))

(* same, but for a single element *)
let get_side_without x l r =
  if List.mem x l then r
  else if List.mem x r then l
  else raise (Side_without (x, l, r))

let list_replace ~src ~dst = List.map (fun x -> if x = src then dst else x)

let edge_update ~src ~dst = function
  | Pend(id,bl,l) as p ->
      if List.mem src l then Pend(id,bl, list_replace src dst l) else p
  | Inte(bl,l,r) as i ->
      if not ((List.mem src l) || (List.mem src r)) then i
      else Inte(bl, list_replace src dst l, list_replace src dst r)


(* *** PTREE UTILS *** *)

let create = Hashtbl.create
let fold = Hashtbl.fold
let iter = Hashtbl.iter
let strict_replace pt i e =
  if Hashtbl.mem pt i then Hashtbl.replace pt i e
  else raise (Missing_edge (i, "strict_replace"))
let find pt i =
  try Hashtbl.find pt i with
  | Not_found -> raise (Missing_edge (i, "find"))
let strict_remove pt i =
  if Hashtbl.mem pt i then Hashtbl.remove pt i
  else raise (Missing_edge (i, "strict_remove"))
let safe_add h k v =
  assert(not (Hashtbl.mem h k));
  Hashtbl.add h k v
let tree_update ~src ~dst pt id =
  if src = id then raise (Not_implemented "replacing own id");
  strict_replace pt id (edge_update ~src ~dst (find pt id))

(* uuuuuugly! *)
exception Found_inte of int
let find_internal pt =
  try
    iter
      (fun i -> function
        | Inte(_,_,_) -> raise (Found_inte i)
        | _ -> ())
      pt;
    None
  with
  | Found_inte i -> Some i

(* Define a node of an edge to be the
 * (edge id)::(one of the edge lists of the node)
 * Here we check if n is one of the nodes of the edge with id eid.
 * *)
let check_node n eid = function
  | Pend(_,_,l) ->
      let found = eid::l in
      if not (sorted_list_eq n found) then
        raise (Pend_mismatch (n,found))
  | Inte(_,l,r) ->
      let foundl,foundr = (eid::l,eid::r) in
      if not ((sorted_list_eq n foundl) || (sorted_list_eq n foundr)) then
        raise (Inte_mismatch (n,foundl,foundr))

(* check all of the connections of a tree *)
let check pt =
  let rec rooted_check above eidl =
    let our_node = above::eidl in
    List.iter
      (fun below ->
        let e = try find pt below with
        | Missing_edge (missing,_) as x ->
            Printf.printf "%d missing (requested by %d)\n" missing above;
            raise x
        in
        check_node our_node below e;
        match e with
        | Pend(_,_,_) -> ()
        | Inte(_,l,r) -> rooted_check below (get_side_without above l r))
      eidl
  in
  match find_internal pt with
  | None -> assert false
  | Some i -> begin
      match find pt i with
      | Inte(_,l,r) -> rooted_check i l; rooted_check i r
      | _ -> assert false
    end

(* *** OF *** *)

(* bl_getter is a function which returns a bl given an id *)
let of_stree bl_getter st =
  let pt = create (1+(Stree.max_id st)) in
  (* righto is None if we have a pendant edge *)
  let add_edge id left righto =
    let bl = bl_getter id in
    safe_add pt id
      (match righto with
      | None -> Pend(id, bl, left)
      | Some right -> Inte(bl, left, right))
  in
  let rec aux above_ids = function
    | Stree.Node(id, tL) ->
        let ids_of_tl = List.map Stree.top_id in
        add_edge id above_ids (Some (ids_of_tl tL));
        List.iter
          (fun (below, rest) -> aux (id::(ids_of_tl rest)) below)
          (ListFuns.pull_each_out tL)
    | Stree.Leaf id -> add_edge id above_ids None
  in
  (* the basic tree builder, which doesn't take tricky rooting into account *)
  let root_build (to_build, rest) =
    aux (List.map Stree.top_id rest) to_build
  in
  let () =
    match st with
    | Stree.Leaf _ -> ()
    | Stree.Node(_, [t1; t2]) -> begin
      (* tree with degree two rootings require some special care *)
      (* first build the basic tree *)
      List.iter root_build (ListFuns.pull_each_out [t1; t2]);
      let (id1, id2) = (Stree.top_id t1, Stree.top_id t2) in
      match (find pt id1, find pt id2) with
      | (Inte(bl1,l1,r1), Inte(bl2,l2,r2)) ->
          let join1 = get_other_side [id2] l1 r1
          and join2 = get_other_side [id1] l2 r2
          in
          (* make new internal edge *)
          strict_replace pt id1 (Inte(bl1+.bl2, join1, join2));
          (* clean out old edge *)
          strict_remove pt id2;
          (* reconnect things to new edge *)
          List.iter (tree_update ~src:id2 ~dst:id1 pt) (join1 @ join2);
      | _ -> raise (Not_implemented "rooted on pendant edge")
      end
    | Stree.Node(_, tL) -> List.iter root_build (ListFuns.pull_each_out tL);
  in
  pt

let of_gtree gt =
  let get_bl i =
    try (IntMap.find i gt.Gtree.bark_map)#get_bl with
    | Not_found -> failwith "tree is missing branch lengths"
  in
  of_stree get_bl gt.Gtree.stree

let of_string s = of_gtree (Newick_gtree.of_string s)
let of_file s = of_gtree (Newick_gtree.of_file s)


(* *** TO *** *)

let to_gtree pt =
  (* start with maximum of indices of the ids *)
  let count =
    ref (fold
      (fun _ e j -> match e with Pend(i,_,_) -> max i j | _ -> j)
      pt 0)
  and m = ref IntMap.empty
  in
  let add_bark i bl nameo =
    m :=
      IntMap.add i
        (new Newick_bark.newick_bark (`Of_bl_name_boot(Some bl, nameo, None)))
        !m
  in
  (* above is a neighboring edge index in the "up" direction in the resulting
   * rooting *)
  let rec aux ~above our_id =
    match find pt our_id with
    | Inte(bl,l,r) ->
        incr count;
        let node_id = !count in (* have to nail down count due to recursion *)
        add_bark node_id bl None;
        let our_side = get_side_without above l r in
        Stree.Node(node_id, List.map (aux ~above:our_id) our_side)
    | Pend(id,bl,_) -> add_bark id bl (Some (string_of_int id)); Stree.Leaf id
  in
  match find_internal pt with
  | Some start_edge -> begin
      match find pt start_edge with
      | Inte(bl,l,r) ->
          let stl = aux ~above:(List.hd r) start_edge
          and str = aux ~above:(List.hd l) start_edge
          in
          add_bark (Stree.top_id stl) (bl/.2.) None;
          add_bark (Stree.top_id str) (bl/.2.) None;
          incr count;
          Gtree.gtree (Stree.Node(!count, [stl;str])) !m
      | Pend(_,_,_) -> assert(false)
  end
  | None ->
      let tL = (* fix our mutables *)
        fold
          (fun _ e l ->
            match e with
            | Inte(_,_,_) -> assert false
            | Pend(id,bl,_) ->
                add_bark id bl (Some (string_of_int id));
                (Stree.Leaf id)::l)
          pt
          []
      in
      Gtree.gtree (Stree.Node (1 + !count, tL)) !m

let to_stree pt = Gtree.get_stree (to_gtree pt)
