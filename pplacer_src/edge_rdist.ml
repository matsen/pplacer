(* Let A and B be two edges in a tree. let C be their common ancestor. we say
 * that A and B are "serial" if C is one of A or B, and "parallel" if not.
 *
 * This module supports two kinds of calculation: length of common ancestry, and
 * pairwise distances. The idea is that we want to be able to compute distances
 * between things and common ancestry without doing a tree traversal.
 *
 * The pairwise distances are standard distal-side distances except that they
 * have this Parallel versus Serial thing.
 *
 * For common ancestry, for each pair of edges we store the distance from the
 * root to their common ancestor, with an indication if the relationship between
 * the edges is serial or parallel. If they are parallel, then the distance from
 * the common ancestor of two placements on those edge to the root is just the
 * included value. If they are serial, then we need to add in the distance from
 * the placement to the proximal side of the placement edge.
 *
 * The v part of a ca_info gives the distance from the root to the distal side
 * of the edge. This "diagonal" part is needed when we have two placements on
 * the same edge.
 *
 * NOTE: this function assumes that edges are numbered in a depth first manner,
 * such that edge numbering strictly increases from a leaf to the root.
*)


open Ppatteries

type relation_dist = Parallel of float | Serial of float

let ppr_rdist ff = function
 | Parallel x -> Format.fprintf ff "P%g" x
 | Serial x ->   Format.fprintf ff "S%g" x

let ppr_rdist_uptri ff u = Uptri.ppr_uptri ppr_rdist ff u


(* ***** DISTANCE ***** *)

(* Build a matrix of distances between distal sides of edges with Serial and
 * Parallel labels (i.e. m_ij is the distance between the distal sides of the
 * ith and jth edges. Note that we assume that there are no placements on the
 * root edge, and so do not have an entry in our uptri for it.
 * *)
let build_pairwise_dist t =
  let stree = Gtree.get_stree t in
  let u = Uptri.create (Stree.n_edges stree) (Parallel 0.) in
  (* set all pairs of the below with (Parallel distance) *)
  let parallel_set below =
    ListFuns.list_iter_over_pairs_of_single
      (ListFuns.list_iter_over_pairs_of_two
        (fun (i,di) (j,dj) -> Uptri.set u i j (Parallel (di+.dj))))
      below
  in
  let rec aux = function
    | Stree.Node(id, tL) ->
        let curr_bl = Gtree.get_bl t id in
        let below = List.map aux tL in
        parallel_set below;
        let flat_below = List.flatten below in
    (* note that we don't include the current distance in the serial edge
     * (we are calculating to the proximal side) *)
        List.iter
          (fun (i,d) -> Uptri.set u i id (Serial d))
          flat_below;
        (id, curr_bl)::
          (List.map (fun (i,d) -> (i, d +. curr_bl)) flat_below)
    | Stree.Leaf id ->
        [id, Gtree.get_bl t id]
  in
  (* avoid root edge *)
  match stree with
    | Stree.Node(_, tL) ->
        parallel_set (List.map aux tL);
        u
    | Stree.Leaf _ -> failwith "unexpected leaf at root of tree"

(* Find the distance between two locations on the tree. *)
let find_pairwise_dist rdist_uptri edge1 distal1 edge2 distal2 =
  if edge1 = edge2 then abs_float (distal1  -. distal2)
  else match Uptri.get_loose rdist_uptri edge1 edge2 with
  | Parallel x ->
      x -. distal1 -. distal2
  | Serial x ->
      if edge1 > edge2 then x +. distal1 -. distal2
      else x +. distal2 -. distal1


(* ***** COMMON ANCESTRY (not explicitly unit tested) ***** *)

type ca_info =
  { u : relation_dist Uptri.uptri;
    v : float array; }

(* make an uptri with (i,j) entry equal to the distance from the root to the
 * MRCA of edges numbered i and j with Parallel and Serial as above.
 *
 * note that we assume that there are no placements on the root edge, and so do
 * not have an entry in our uptri for it.
 * *)
let build_ca_info t =
  let stree = Gtree.get_stree t in
  let u = Uptri.create (Stree.n_edges stree) (Parallel 0.)
  and v = Array.make (Stree.n_edges stree) 0. in
  (* set all pairs of the below with (Parallel curr_dist) *)
  let parallel_set below curr_dist =
    ListFuns.list_iter_over_pairs_of_single
      (ListFuns.list_iter_over_pairs_of_two
        (fun i j -> Uptri.set u i j (Parallel curr_dist)))
      below
  in
  let set_v i x = Array.set v i x in
  let rec aux dist_to_root = function
    | Stree.Node(id, tL) ->
        let curr_dist = dist_to_root +. Gtree.get_bl t id in
        let below = List.map (aux curr_dist) tL in
        (* note that for the below ones we want to use curr_dist, which includes
         * the length of the current edge *)
        parallel_set below curr_dist;
        let flat_below = List.flatten below in
        (* whereas in the serial case we don't want to include current edge *)
        List.iter
          (fun i -> Uptri.set u i id (Serial curr_dist))
          flat_below;
        set_v id curr_dist;
        id::flat_below
    | Stree.Leaf id ->
        set_v id (dist_to_root +. Gtree.get_bl t id);
        [id]
  in
  (* avoid root edge *)
  match stree with
    | Stree.Node(_, tL) ->
        parallel_set (List.map (aux 0.) tL) 0.;
        { u = u; v = v; }
    | Stree.Leaf _ -> failwith "unexpected leaf at root of tree"

(* find the distance to the common ancestor given info from two placements *)
let find_ca_dist ca_info (edge1, distal1) (edge2, distal2) =
  if edge1 = edge2 then
    ca_info.v.(edge1) -. (max distal1 distal2)
  else match Uptri.get_loose ca_info.u edge1 edge2 with
  | Parallel x ->
      (* the common ancestor is distal to both *)
      x
  | Serial x ->
  (* if edge 1 is on top, then we need to subtract the distal part of its
   * placement from the common ancestry *)
      x -. (if edge1 > edge2 then distal1 else distal2)

