open Ppatteries
open Stree

type color = Tax_id.t
module ColorSet = Tax_id.TaxIdSet
module ColorMap = Tax_id.TaxIdMap
type cset = ColorSet.t
type 'a cmap = 'a ColorMap.t

let ppr_csetim = IntMap.ppr_gen ColorSet.ppr

module OrderedColorSet = struct
  type t = cset
  let compare = ColorSet.compare
end

module PprColorSet = struct
  type t = cset
  let ppr = ColorSet.ppr
end

module ColorSetMap = BetterMap (Map.Make(OrderedColorSet)) (PprColorSet)

type coloropt = color option
module OrderedColorOpt = struct
  type t = coloropt
  let compare co1 co2 =
    match co1, co2 with
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0
      | Some c1, Some c2 -> compare c1 c2
end

module PprColorOpt = struct
  type t = coloropt
  let ppr ff = function
    | Some c -> Format.fprintf ff "<%s>" (Tax_id.to_string c)
    | None -> Format.fprintf ff "--"
end

module ColorOptSet = BetterSet (Set.Make(OrderedColorOpt)) (PprColorOpt)
module ColorOptMap = BetterMap (Map.Make(OrderedColorOpt)) (PprColorOpt)

(* A question is a pair (c, X) where c is an arbitrary optional color and X is a
 * subset of the cut set for the edge above the given internal node. The
 * terminology comes from wanting to know the best solution for such a subtree
 * where the colors of that cut set are restricted to X, and there is a leaf of
 * color c attached to the root of the subtree. *)
type question = color option * cset

module PprQuestion = struct
  type t = question
  let ppr ff (co, cs) =
    Format.fprintf ff "@[(%s,@ " begin match co with
      | None -> "-"
      | Some c -> Tax_id.to_string c
    end;
    ColorSet.ppr ff cs;
    Format.fprintf ff ")@]"
end

module OrderedQuestion = struct
  type t = question
  let compare (co1, cs1) (co2, cs2) =
    match co1, co2 with
      | Some c1, Some c2 when c1 = c2 ->
        ColorSet.compare cs1 cs2
      | Some c1, Some c2 -> compare c1 c2
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> ColorSet.compare cs1 cs2
end

module QuestionMap = BetterMap (Map.Make(OrderedQuestion)) (PprQuestion)
type 'a qmap = 'a QuestionMap.t

type csetl = ColorSet.t list
type apart = color option * csetl  (* apart = almost partition *)
type sizem = int ColorMap.t
type colorm = color IntMap.t
type cdtree = colorm * stree
type local_phi = (apart * int) QuestionMap.t
type phi = local_phi IntMap.t
(* nu_f is a type for upper bounds for the number of leaves left in a convex
 * subset of leaves. We pass them phi, the apart, and the list of top indices
 * for the subtrees at our internal node. *)
type nu_f = cset -> sizem list -> apart -> int

(* Abbreviations *)
module CS = ColorSet
module COS = ColorOptSet
module COM = ColorOptMap
module CSM = ColorSetMap

let all colors = List.fold_left CS.union CS.empty colors
let between colors = all
  (List.map
     (fun (x, y) -> CS.inter x y)
     (ListFuns.list_pairs_of_single colors))

let build_sizemim_and_cutsetim (colors, tree) =
  (* Building an internal_node -> szm, color_below map. *)
  let rec aux = function
    | Leaf i ->
      let szm, clbelow = match begin
        try
          Some (IntMap.find i colors)
        with
          | Not_found -> None
      end with
        | Some color ->
          let szm = ColorMap.singleton color 1
          and clbelow = CS.singleton color in
          szm, clbelow
        | None ->
          ColorMap.empty, CS.empty
      in
      szm, clbelow, IntMap.singleton i (szm, clbelow)
    | Node (i, subtrees) ->
      let maps = List.map aux subtrees in
      let szm = ColorMap.merge_counts (List.map (fun (a, _, _) -> a) maps) in
      let clbelow, leafm = List.fold_left
        (fun (claccum, lfaccum) (_, cl, lf) ->
          CS.union claccum cl, IntMap.union lfaccum lf)
        (CS.empty, IntMap.empty)
        maps
      in
      szm, clbelow, IntMap.add i (szm, clbelow) leafm
  in
  let _, _, leafm = aux tree in
  let szm = IntMap.map fst leafm
  and below_clm = IntMap.map snd leafm
  in
  (* Refines the below_clm to just map to the cut colors.
   * The procedure is to erase non-between colors as we proceed down the tree.
   * Accum is the partially-erased color set IntMap.
   * Terminated are the colors for which exist on the "above" side of this
   * internal node in the tree-- thus if we see a terminated color below then we
   * know that the color is cut by the edge above this node.
   * *)
  let rec aux terminated accum = function
    | Leaf _ -> accum
    | Node (_, subtrees) ->
      let colorsets = List.map
        (fun tree -> IntMap.find (top_id tree) accum)
        subtrees
      in
      (* Update terminated. *)
      let terminated' = CS.union terminated (between colorsets) in
      List.fold_left2
        (fun accum colors tree ->
          let i = top_id tree in
          (* colors' are just those edge colors in terminated' *)
          let colors' = CS.inter colors terminated' in
          if colors = colors' then
          (* We don't have to cut anything from any of the edges below because
           * we know that every color below also exists "above" this edge. *)
            accum
          else
            aux
              terminated'
              (IntMap.add i colors' accum)
              tree)
        accum
        colorsets
        subtrees
  in
  let cut_clm = aux CS.empty below_clm tree in
  szm, cut_clm

let subtreelist_map f tree =
  let rec aux accum = function
    | [] -> accum
    | Leaf i :: rest ->
      aux
        (IntMap.add i [f i] accum)
        rest
    | Node (i, subtrees) :: rest ->
      aux
        (IntMap.add
           i
           (List.map
              (fun tree -> f (top_id tree))
              subtrees)
           accum)
        (List.rev_append subtrees rest)
  in
  aux IntMap.empty [tree]

let maplist_of_map_and_tree map =
  subtreelist_map (fun i -> IntMap.find i map)

(* Cartesian product of a list list. *)
let product lists =
  let rec aux accum base = function
    | [] -> (List.rev base) :: accum
    | l :: rest ->
      List.fold_left
        (fun accum x -> aux accum (x :: base) rest)
        accum
        l
  in
  aux [] [] lists

(* Find the potential distributions of a color across a list of cut sets. That
 * is, we take the cartesian product of I_i for every i, where i is [{}] if the
 * color isn't in the ith cut set, and [{color}, {}] if it is. *)
let cutsetdist cutsetl ?(allow_multiple = false) color =
  (* We recur over the cut sets below our internal node.
   * Base is just a list of empty sets of the correct length. *)
  let rec aux base accum = function
    | [] -> List.map List.rev accum
    | cutset :: rest ->
      let accum = List.fold_left
        (fun accum x ->
          let accum' = (CS.empty :: x) :: accum in
          if allow_multiple && CS.mem color cutset then
            (CS.singleton color :: x) :: accum'
          else
            accum')
        []
        accum
      in
      let accum =
        if CS.mem color cutset then
          (CS.singleton color :: base) :: accum
        else
          accum
      in
      aux (CS.empty :: base) accum rest
  in
  aux [] [] cutsetl

(* Transpose a list of lists, then fold a function along the new list of lists.
 * e.g. transposed_fold (+) [0; 0] [[1; 2]; [3; 4]] -> [4; 6] *)
let transposed_fold f start ll =
  let rec aux prev = function
    | [] -> prev
    | l :: rest ->
      aux
        (List.map2 f prev l)
        rest
  in
  aux start ll

let coptset_of_cset cset =
  CS.fold (fun c s -> COS.add (Some c) s) cset COS.empty

let cset_of_coptset coptset =
  COS.fold
    (fun c s ->
      match c with
        | Some c' -> CS.add c' s
        | None -> s)
    coptset
    CS.empty

(* As indicated by the underscore, this function is not designed to work as is.
 * Indeed, we need to preprocess with the case of c not being in any of the cut
 * sets under the internal node as defined in build_apartl below. *)
let _build_apartl strict cutsetl kappa (c, x) =
  let xopt = coptset_of_cset x in
  (* Anything in kappa - x doesn't get distributed. *)
  let to_exclude = coptset_of_cset (CS.diff kappa x) in
  (* The potential b's for our apartl. *)
  let potential_bs =
    (* Because xopt never contains None, this is in fact testing c in x. If that
     * is true, b will only be c, since c is added to potential_bs later.
     * *)
    if COS.mem c xopt then
      COS.empty
    else
      COS.add None (COS.diff (coptset_of_cset (between cutsetl)) to_exclude)
  in
  (* These are the colors that we need to put in the different subsets. *)
  let to_distribute = COS.union
    xopt
    (COS.diff (coptset_of_cset (all cutsetl)) to_exclude)
  in
  let apartl = COS.fold
    (* Fold over the possible values of b: the color of the internal node. *)
    (fun b accum ->
      (* The colors are distributed in two steps. Note that any color except for
       * b can only occur once in a given pi. Thus we first find the potential
       * distributions of the to_distribute colors (except for b) into the
       * cut sets below our internal node. We find these distributions one at a
       * time, then take the union below. *)
      let dist = List.map
        (cutsetdist cutsetl)
        (CS.elements (cset_of_coptset (COS.remove b to_distribute)))
      in
      (* Next make every distribution with {} with {b} if b is in the cut set *)
      let dist = match b with
        | Some b' -> cutsetdist cutsetl ~allow_multiple:true b' :: dist
        | None -> dist
      and startsl = List.rev_map (fun _ -> CS.empty) cutsetl in
      (* Finish off the meat of the recursion by mapping with union over the
       * cartesian product of the single-color distributions. *)
      let pis = List.map
        (transposed_fold CS.union startsl)
        (product dist)
      in
      (* By the construction of the pis, between pi can only be empty or b.
       * In the case of strict convexity, we require all pi to be just the set
       * {b} if b is not None.
       * In the usual case, we filter out those aparts such that b is not c or
       * None. Recall (see the intro to convex.mli) that None represents any
       * color that is not "forced" by convexity considerations. c is None when
       * there is not an above color that is in x. b is None when c is None and
       * there are no colors shared between the pi_i.
       *)
      let is_valid =
        if strict then
          let b' = COS.singleton b |> cset_of_coptset in
          fun pi -> c = None || (b = c && CS.subset (all pi) b')
        else
          fun pi -> not (CS.is_empty (between pi)) || b = c || b = None
      in
      List.fold_left
        (fun accum pi ->
          if is_valid pi then (b, pi) :: accum
          else accum)
        accum
        pis)
    (* We add c to the list of things that can be colors of internal nodes. *)
    (COS.add c potential_bs)
    []
  in
  apartl

let build_apartl_memo = Hashtbl.create 1024

(* The primary apartl builder.
 * Cutsetl is the list of cut sets below, kappa are those sets colors cut from
 * the internal node above.
 * While this function is mostly a wrapper around the apartl memo, it /does/
 * also update `c`, as it's useful to do so pre-memoization. *)
let build_apartl ?(strict = false) cutsetl kappa (c, x) =
  (* If c is not in any of the cut sets below and we're not building strict
   * apartls, then we can replace it with None. *)
  let c = match c with
    | Some c' when not strict && not (List.exists (CS.mem c') cutsetl) -> None
    | x -> x
  in
  let q = c, x in
  try
    Hashtbl.find build_apartl_memo (cutsetl, kappa, q)
  with
    | Not_found ->
      let ret = _build_apartl strict cutsetl kappa q in
      Hashtbl.add build_apartl_memo (cutsetl, kappa, q) ret;
      ret

let apart_nu kappa sizeml (b, pi) =
  let kappa = match b with
    | Some c -> ColorSet.remove c kappa
    | None -> kappa
  in
  List.fold_left2
    (fun accum cutset sizem ->
      let to_ignore = ColorSet.diff kappa cutset in
      let is_ignored c = ColorSet.mem c to_ignore in
      ColorMap.fold
        (fun color count accum ->
          if is_ignored color then accum else accum + count)
        sizem
        accum)
    0
    pi
    sizeml

let add_phi node question answer phi =
  let local_phi =
    try
      IntMap.find node phi
    with
      | Not_found -> QuestionMap.empty
  in
  IntMap.add node (QuestionMap.add question answer local_phi) phi

let null_apart = None, []

let rec phi_recurse ?strict ?nu_f cutsetim sizemlim tree ((_, x) as question) phi =
  let i = top_id tree in
  match begin
    try
      Some (QuestionMap.find question (IntMap.find i phi))
    with
      | Not_found -> None
  end with
    | Some (_, omega) -> phi, omega
    | None ->
  (* Begin real work. *)
  let phi_recurse = phi_recurse ?strict ?nu_f cutsetim sizemlim in
  let phi, res = match tree with
    | Leaf _ ->
      (* Could put in some checks here about the size of cutsetim. *)
      let omega = if x = IntMap.find i cutsetim then 1 else 0 in
      phi, Some (omega, null_apart)
    | Node (_, subtrees) ->
      let cutsetl = List.map
        (fun subtree -> IntMap.find (top_id subtree) cutsetim)
        subtrees
      in
      let apartl = build_apartl
        ?strict
        cutsetl
        (IntMap.find i cutsetim)
        question
      in
      (* Recur over subtrees to calculate (omega, updated_phi) for the apart
       * (b, pi). *)
      let apart_omega phi (b, pi) =
        List.fold_left2
          (fun (phi, subtotal) pi_i subtree ->
            let phi, omega = phi_recurse subtree (b, pi_i) phi in
            phi, subtotal + omega)
          (phi, 0)
          pi
          subtrees
      in
      let nu_apartl = match nu_f with
        | None -> List.map (fun apart -> None, apart) apartl
        | Some nu_f ->
          let apart_nu' = nu_f
            (IntMap.find i cutsetim)
            (IntMap.find i sizemlim)
          in
          let nu_apartl = List.map
            (fun apart -> apart_nu' apart, apart)
            apartl
          in
          List.rev_map (fun (a, b) -> Some a, b) (List.sort compare nu_apartl)
      in
      let rec aux phi current_best = function
        | (nu_opt, apart) :: rest -> (
          let phi, omega = apart_omega phi apart in
          match current_best, nu_opt with
            | None, _ -> aux phi (Some (omega, apart)) rest
            | Some (prev_omega, _), _ when omega > prev_omega ->
              aux phi (Some (omega, apart)) rest
            | Some (prev_omega, _), Some nu when nu < prev_omega ->
              phi, current_best
            | _, _ -> aux phi current_best rest)
        | [] -> phi, current_best
      in
      aux phi None nu_apartl
  in
  match res with
    | Some (omega, apart) ->
      let phi' = add_phi i question (apart, omega) phi in
      phi', omega
    | None ->
      (* In this case, there's no viable candidates below this node on the
       * tree. The important part is that since there's no viable apart, this
       * can't be stored in phi. So: if a question is not found in the final
       * phi, the answer is "ignore this node and all below it." *)
      phi, 0

let badness cutsetim =
  IntMap.fold
    (fun _ cutset (worst, total) ->
      let badness_i = max 0 ((CS.cardinal cutset) - 1) in
      max worst badness_i, total + badness_i)
    cutsetim
    (0, 0)

(* From an stree, produce a map from all the node numbers in the stree to sets
 * of all of the node numbers below that respective node. *)
let rec belowm_of_stree = function
  | Leaf i -> IntSet.singleton i |> IntMap.singleton i
  | Node (i, subtrees) ->
    let map = List.map belowm_of_stree subtrees |> List.reduce IntMap.union in
    IntMap.add
      i
      (IntMap.values map |> Enum.reduce IntSet.union |> IntSet.add i)
      map

let prune_tree (colors, st) =
  let kept_leaves = IntMap.keys colors |> IntSet.of_enum
  and belowm = belowm_of_stree st
  and all_leaves = leaf_ids st |> IntSet.of_list in
  let rec should_keep i =
    IntMap.find i belowm
      |> IntSet.disjoint kept_leaves
      |> not
  and aux = function
    | Leaf _ as l -> l
    | Node (i, subtrees) ->
      List.filter_map
        (fun t -> let j = top_id t in
          if not (should_keep j) then None
          else Some (aux t))
        subtrees
      |> node i
  in
  IntSet.diff all_leaves kept_leaves, aux st

let solve ?strict ?nu_f ((_, tree) as cdtree) =
  let sizemim, cutsetim = build_sizemim_and_cutsetim cdtree in
  let cutsetim = IntMap.add (top_id tree) CS.empty cutsetim in
  let sizemlim = maplist_of_map_and_tree sizemim tree in
  Hashtbl.clear build_apartl_memo;
  phi_recurse ?strict ?nu_f cutsetim sizemlim tree (None, CS.empty) IntMap.empty

(* Given a phi (an implicit solution) get an actual solution, i.e. a subset of
 * the leaves to include. The recursion works as follows: maintain rest, which
 * is the list of things that remain to be expanded. If the first element of
 * rest has a leaf, then add it to the list if appropriate and recur. If not,
 * expand out the subtrees of the first element of rest and recur. *)
let nodeset_of_phi_and_tree phi tree =
  let rec aux accum = function
    | (Leaf i, question) :: rest ->
      let _, omega = QuestionMap.find question (IntMap.find i phi) in
      let accum =
        if omega = 0 then
          accum
        else
          IntSet.add i accum
      in
      aux accum rest
    | (Node (i, subtrees), question) :: rest ->
      let qmap = IntMap.find i phi in
      (* As above, if a question isn't found in the final phi, we should be
       * ignoring this node and all the nodes below it. *)
      if not (QuestionMap.mem question qmap) then aux accum rest else
        let (b, pi), _ = QuestionMap.find question qmap in
        let rest' = List.fold_left2
          (fun rest pi_i subtree -> (subtree, (b, pi_i)) :: rest)
          rest
          pi
          subtrees
        in
        aux accum rest'
    | [] -> accum
  in
  aux IntSet.empty [tree, (None, CS.empty)]

(* Make map from names to their respective indices. *)
let node_label_map_of_tree t =
  Gtree.fold_over_leaves
    (fun i bark accum ->
      try
        StringMap.add bark#get_node_label i accum
      with Newick_bark.No_node_label -> accum)
    t
    StringMap.empty

let build_rank_tax_map td node_fn enum =
  let add_to_rankmap seq rankmap ti =
    match node_fn seq with
      | Some node ->
        let rank = Tax_taxonomy.get_tax_rank td ti in
        let seqmap = IntMap.get rank IntMap.empty rankmap in
        IntMap.add
          rank
          (IntMap.add node ti seqmap)
          rankmap
      | None -> rankmap
  in
  Enum.fold
    (fun rankmap (seq, ti) ->
      List.fold_left
        (add_to_rankmap seq)
        rankmap
        (Tax_taxonomy.get_lineage td ti))
    IntMap.empty
    enum

let rank_tax_map_of_refpkg rp =
  let node_map = Refpkg.get_ref_tree rp |> node_label_map_of_tree
  and td = Refpkg.get_taxonomy rp in
  Refpkg.get_seqinfom rp
  |> StringMap.enum
  |> Enum.map (second (fun {Tax_seqinfo.tax_id} -> tax_id))
  |> build_rank_tax_map td (flip StringMap.Exceptionless.find node_map)
  |> tap (fun m ->
    Array.iteri
      (fun rank rankname ->
        if not (IntMap.mem rank m) then
          dprintf "warning: rank %s not represented in the lineage of any \
                   sequence in reference package %s.\n"
            rankname
            (Refpkg.get_name rp))
      td.Tax_taxonomy.rank_names)

let add_color_setly k v m =
  IntMap.add k (CS.add v (IntMap.get k CS.empty m)) m

let merge_color_setly m1 m2 =
  IntMap.fold
    (fun k v m ->
      IntMap.add k (CS.union v (IntMap.get k CS.empty m)) m)
    m1
    m2

let alternate_colors ((colors, tree) as cdtree) =
  let _, cutsetim = build_sizemim_and_cutsetim cdtree in
  let cutsetlim = maplist_of_map_and_tree cutsetim tree in
  let rec aux accum counter = function
    | [] -> accum
    | (group, cur_tree) :: rest ->
      let i = top_id cur_tree in
      let cutsetl = IntMap.find i cutsetlim in
      let cutset = all cutsetl in
      assert (CS.cardinal cutset <= 1);
      let node_relevant = CS.is_empty cutset in
      let accum', counter', group' = match group, cur_tree with
        | Some g, Leaf _ ->
          IntMap.add_listly g (i, true) accum, counter, group
        | None, Leaf _ when not (IntMap.mem i colors) ->
          IntMap.add_listly counter (i, true) accum, counter + 1, Some counter
        | Some g, Node _ when node_relevant ->
          IntMap.add_listly g (i, false) accum, counter, group
        | None, Node _ when node_relevant ->
          IntMap.add_listly counter (i, false) accum, counter + 1, Some counter
        | _ -> accum, counter, None
      in
      let rest' = match cur_tree with
        | Leaf _ -> rest
        | Node (_, subtrees) ->
          List.fold_left
            (fun accum subtree -> (group', subtree) :: accum)
            rest
            subtrees
      in
      aux accum' counter' rest'
  in
  let groupmap = aux IntMap.empty 0 [None, tree] in
  let rev_groupmap = IntMap.fold
    (fun group entries accum ->
      List.fold_left
        (fun accum (i, _) -> IntMap.add i group accum)
        accum
        entries)
    groupmap
    IntMap.empty
  in
  let rec aux accum = function
    | [] -> accum
    | (_, Leaf _) :: rest -> aux accum rest
    | (parent, (Node (i, subtrees) as cur_tree)) :: rest ->
      let accum' =
        if IntMap.mem i rev_groupmap then
          let g = IntMap.find i rev_groupmap in
          let subtrees' = match parent with
            | Some parent -> parent :: subtrees
            | None -> subtrees
          in
          List.fold_left
            (fun accum -> function
              | Leaf i | Node (i, _) ->
                merge_color_setly
                  accum
                  (IntMap.singleton g (all (IntMap.find i cutsetlim))))
            accum
            subtrees'
        else
          List.fold_left
            (fun accum -> function
              | Leaf j when IntMap.mem j rev_groupmap ->
                let g = IntMap.find j rev_groupmap in
                merge_color_setly
                  accum
                  (IntMap.singleton g (all (IntMap.find i cutsetlim)))
              | _ -> accum)
            accum
            subtrees
      in
      let parent' = Some cur_tree in
      let rest' = List.fold_left
        (fun accum subtree -> (parent', subtree) :: accum)
        rest
        subtrees
      in
      aux accum' rest'
  in
  let group_colors = aux IntMap.empty [None, tree] in
  IntMap.fold
    (fun g entries accum ->
      let g_colors = IntMap.get g CS.empty group_colors in
      List.fold_left
        (fun accum (i, is_leaf) ->
          if is_leaf then
            IntMap.add i g_colors accum
          else
            accum)
        accum
        entries)
    groupmap
    IntMap.empty


let copt_singleton = function
  | Some color -> CS.singleton color
  | None -> CS.empty

let add_longest k v m =
  match begin
    try
      Some (CSM.find k m)
    with Not_found -> None
  end with
    | Some prev when IntSet.cardinal v <= IntSet.cardinal prev -> m
    | _ -> CSM.add k v m

module Naive = struct

  let solve ((_, tree) as cdtree) =
    let _, cutsetim = build_sizemim_and_cutsetim cdtree in
    let cutsetim = IntMap.add (top_id tree) CS.empty cutsetim in
    let cutsetlim = maplist_of_map_and_tree cutsetim tree in

    let rec walk = function
      | Leaf i ->
        let kappa = IntMap.find i cutsetim in
        if CS.is_empty kappa then
          COM.singleton None (CSM.singleton CS.empty (IntSet.singleton i))
        else
          let color = CS.choose kappa in
          let csm = CSM.singleton (CS.singleton color) (IntSet.singleton i) in
          COM.of_pairlist [Some color, csm; None, csm]
      | Node (i, subtrees) ->
        let phi = List.map walk subtrees
        and big_b = between (IntMap.find i cutsetlim)
        and kappa = IntMap.find i cutsetim in
        let ret = COS.fold
          (fun c accum ->
            let c' = copt_singleton c in
            let ret_c = COS.fold
              (fun b accum ->
                let b' = copt_singleton b in
                let rec aux used_colors used_nodes accum = function
                  | [] -> add_longest used_colors used_nodes accum
                  | phi_i :: rest ->
                    let accum' = aux used_colors used_nodes accum rest in
                    let x_is = match COM.get b CSM.empty phi_i with
                      | x when CSM.is_empty x -> COM.find None phi_i
                      | x -> x
                    in
                    CSM.fold
                      (fun x_i nodes accum ->
                        if CS.is_empty (CS.diff (CS.inter x_i used_colors) b')
                          && (b = c || CS.is_empty (CS.inter x_i c'))
                        then
                          aux
                            (CS.union used_colors x_i)
                            (IntSet.union used_nodes nodes)
                            accum
                            rest
                        else
                          accum)
                      x_is
                      accum'
                in
                aux CS.empty IntSet.empty accum phi)
              (COS.add c (coptset_of_cset big_b))
              CSM.empty
            in
            COM.add c ret_c accum)
          (COS.add None (coptset_of_cset kappa))
          COM.empty
        in
        if CS.is_empty kappa then
          let best = CSM.fold
            (fun _ cur -> function
              | Some prev when IntSet.cardinal cur <= IntSet.cardinal prev ->
                Some prev
              | _ -> Some cur)
            (COM.find None ret)
            None
          in
          match best with
            | Some best -> COM.singleton None (CSM.singleton CS.empty best)
            | None -> failwith "no clades on an internal node ???"
        else
          ret

    in
    CSM.find CS.empty (COM.find None (walk tree))

end


