open Ppatteries
open Stree

type t = int * IntSet.t

type fig_state = {
  max_bl: float option;
  edges: IntSet.t;
  rep_edge: int;
  accum: t list;
}

type fold_state = {
  bls: float list;
  all_edges: IntSet.t;
  rep_opt: (int * int) option;
  maybe_figs: t list;
  tot_accum: t list;
}

let add_edge_to_figs i fl =
  List.map (IntSet.add i |> second) fl

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

let figs_of_gtree cutoff gt =
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
      let rep_edge = Option.get rep_opt |> fst
      and accum = add_edge_to_figs i tot_accum in
      match List.sort (flip compare) bls with
        | [] -> {max_bl = None; edges; rep_edge; accum}
        | [bl] when i <> top ->
          {max_bl = Some (bl +. get_bl i); edges; rep_edge; accum}
        | bl1 :: bl2 :: _ when bl1 +. bl2 <= cutoff && i <> top ->
          {max_bl = Some (bl1 +. get_bl i); edges; rep_edge; accum}
        | _ ->
          {max_bl = None; edges = IntSet.empty; rep_edge;
           accum = add_edge_to_figs i maybe_figs |> List.append accum}
  in
  (Gtree.get_stree gt |> aux).accum

let enum_by_score figl score =
  let yielded = ref IntSet.empty in
  List.map (first score) figl
    |> List.sort (comparing fst)
    |> List.enum
    |> Enum.map
        (fun (_, edges) ->
          IntSet.diff edges !yielded
            |> tap (fun _ -> yielded := IntSet.union edges !yielded)
            |> IntSet.elements
            |> List.map (score &&& identity)
            |> List.sort (comparing fst)
            |> List.enum
            |> Enum.map snd)
    |> Enum.flatten

module XXX = Decor_gtree
module XXY = Visualization
