open Ppatteries
open Stree

(* A pair of (representative edge, all edges). *)
type fig = int * IntSet.t

(* Dummy is a fig collection without any actual figs; it just implements the
 * same APIs for the case of not wanting figs. *)
type t =
  | Dummy of int list
  | Figs of fig list

(* This type is used for propagating information up a gtree when determining
 * figs. Just using a tuple gets too complicated too quickly. *)
type fig_state = {
  (* the distance to the most distant leaf, if there are any distal leaves not
   * already in a fig *)
  max_bl: float option;
  (* all of the edges that might become part of this fig *)
  edges: IntSet.t;
  (* the current representative edge *)
  rep_edge: int;
  (* all of the figs that were already determined *)
  accum: fig list;
}

(* This type represents all of the information about the subtrees below an
 * internal node. *)
type fold_state = {
  (* the most distant leaves from all of the subtrees, after removing Nones *)
  bls: float list;
  (* the union of all of the edge sets from the fig_states below this node *)
  all_edges: IntSet.t;
  (* a (representative, leaves below representative) pair; always Some after
   * the fold unless the node had no subtrees *)
  rep_opt: (int * int) option;
  (* a list of all of the subtrees as figs, for the case when all of the
   * subtrees can't share one fig *)
  maybe_figs: fig list;
  (* all of the figs determined by all subtrees *)
  tot_accum: fig list;
}

(** add_edge_to_figs: int -> fig list -> fig list
    Given an edge and a list of figs, add the edge to each fig in the list. *)
let add_edge_to_figs i fl =
  List.map (IntSet.add i |> second) fl

(** fold_figs: (int * fig_state) list -> fold_state
    Part of determining the figs on a tree: at each node, this is used to
    coalesce all of the information about the subtrees together. *)
let fold_figs fl =
  List.fold_left
    (fun accum (j, cur) ->
      let cur_card = IntSet.cardinal cur.edges in
      {bls = maybe_cons cur.max_bl accum.bls;
       all_edges = IntSet.union cur.edges accum.all_edges;
       rep_opt = (match accum.rep_opt with
         | Some (_, prev_card) as prev when prev_card >= cur_card -> prev
         | _ -> Some (j, cur_card));
       maybe_figs = (cur.rep_edge, cur.edges) :: accum.maybe_figs;
       tot_accum = List.append cur.accum accum.tot_accum})
    {bls = []; all_edges = IntSet.empty; maybe_figs = [];
     rep_opt = None; tot_accum = []}
    fl

(** _figs_of_gtree: float -> Newick_gtree.t -> fig list
    Determine all of the figs on a tree such that no two leaves in each fig
    have a distance greater than the cutoff. *)
let _figs_of_gtree cutoff gt =
  let get_bl = Gtree.get_bl gt
  and top = Gtree.top_id gt in
  let rec aux = function
    | Leaf i ->
      {max_bl = Some (get_bl i);
       edges = IntSet.singleton i;
       rep_edge = i;
       accum = []}
    | Node (i, subtrees) ->
      let {bls; all_edges = edges; rep_opt; maybe_figs; tot_accum} =
        List.map (top_id &&& aux) subtrees |> fold_figs
      in
      let accum = if i = top then tot_accum else add_edge_to_figs i tot_accum
      and rep_edge = Option.get rep_opt |> fst in
      match List.sort (flip compare) bls with
        | [] -> {max_bl = None; edges; rep_edge; accum}
        | _ when i = top ->
          {max_bl = None; edges; rep_edge; accum = List.append accum maybe_figs}
        | [bl] ->
          {max_bl = Some (bl +. get_bl i); edges; rep_edge; accum}
        | bl1 :: bl2 :: _ when bl1 +. bl2 <= cutoff ->
          {max_bl = Some (bl1 +. get_bl i); edges; rep_edge; accum}
        | _ ->
          {max_bl = None; edges = IntSet.empty; rep_edge;
           accum = add_edge_to_figs i maybe_figs |> List.append accum}
  in
  (Gtree.get_stree gt |> aux).accum

(** figs_of_gtree: float -> Newick_gtree.t -> t
    Make a collection of figs from a tree, using a Dummy if the cutoff is 0. *)
let figs_of_gtree cutoff gt =
  if cutoff = 0. then
    Dummy (Gtree.nonroot_node_ids gt)
  else
    Figs (_figs_of_gtree cutoff gt)

(** uniquifier: unit -> (IntSet.t -> IntSet.t)
    Create a uniquifier which takes IntSets and returns IntSets which don't
    contain elements from any previously-specified IntSets. *)
let uniquifier () =
  let yielded = ref IntSet.empty in
  fun s ->
    IntSet.diff s !yielded
    |> tap (fun _ -> yielded := IntSet.union s !yielded)

(** _enum_by_score: fig list -> (int -> float) -> float -> (float * int) Enum.t
    Actually enumerate over a list of figs. This first sorts the representative
    edges by the scoring function, then the edges contained in each fig. If
    successive representative scores are within the strike_box, the
    corresponding figs are all merged together before sorting. Each edge is
    yielded with its score. *)
let _enum_by_score figl score_func strike_box =
  let uniquify = uniquifier () in
  let fig_enum = List.map (first score_func) figl
    |> List.sort (comparing fst |> flip)
    |> List.enum
  in
  Enum.map
    (fun (score, edges) ->
      Enum.take_while (fst |- approx_equal ~epsilon:strike_box score) fig_enum
        |> Enum.fold (snd |- IntSet.union |> flip) edges
        |> uniquify
        |> IntSet.elements
        |> List.map (score_func &&& identity)
        |> List.sort (comparing fst |> flip)
        |> List.enum)
    fig_enum
    |> Enum.flatten

(** enum_by_score: (int -> float) -> float -> t -> (float * int) Enum.t
    Enumerate all of the edges in a collection of figs and the score given to
    them by the provided scoring function, roughly ordered by their score. *)
let enum_by_score score strike_box = function
  | Dummy l ->
    List.map (score &&& identity) l
      |> List.sort (comparing fst |> flip)
      |> List.enum
  | Figs fl -> _enum_by_score fl score strike_box

(** enum_all: t -> int Enum.t
    Enumerate all of the edges in collection of figs in an undefined order. *)
let enum_all = function
  | Dummy l -> List.enum l
  | Figs fl ->
    let uniquify = uniquifier () in
    List.enum fl
      |> Enum.map (snd |- uniquify |- IntSet.enum)
      |> Enum.flatten

(** length: t -> int
    The number of figs in a collection of figs. *)
let length = function
  | Dummy _ -> 0
  | Figs fl -> List.length fl

(** onto_decor_gtree: Decor_gtree.t -> t -> Decor_gtree.t
    Decorate a Decor_gtree with all of the figs in the collection of figs. Each
    fig is given a random color, and all of the edges contained within only one
    fig are painted with that color. Edges contained within multiple figs are
    left uncolored. *)
let onto_decor_gtree dt = function
  | Dummy _ -> dt
  | Figs fl ->
    List.fold_left
      (fun dt (_, edges) ->
        let color = Decor.random_color () in
        Decor_gtree.color_clades_above ~color edges dt)
      dt
      fl
    |> Decor_gtree.consolidate_colors Decor_gtree.keep_only_one
