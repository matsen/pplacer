open MapsSets
open Stree

type color = string
module ColorSet = StringSet
module ColorMap = StringMap
type cset = ColorSet.t
type 'a cmap = 'a ColorMap.t

type coloropt = string option
module OrderedColorOpt = struct
  type t = coloropt
  let compare co1 co2 =
    match co1, co2 with
      | None, Some _ -> -1
      | Some _, None -> 1
      | None, None -> 0
      | Some c1, Some c2 -> String.compare c1 c2
end

module PprColorOpt = struct
  type t = coloropt
  let ppr ff = function
    | Some c -> Format.fprintf ff "<%s>" c
    | None -> Format.fprintf ff "--"
end

module ColorOptSet = BetterSet (Set.Make(OrderedColorOpt)) (PprColorOpt)

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
      | Some c -> c
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
      | Some c1, Some c2 -> String.compare c1 c2
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

(* Abbreviations *)
module CS = ColorSet
module COS = ColorOptSet

let all colors = List.fold_left CS.union CS.empty colors
let between colors = all
  (List.map
     (fun (x, y) -> CS.inter x y)
     (Base.list_pairs_of_single colors))

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

let rec powerset = function
  | [] -> [[]]
  | _ :: t as l -> List.fold_left (fun xs t -> l :: t :: xs) [] (powerset t)

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
        (List.rev (List.rev_map2 f prev l))
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

let is_apart (b, pi) x =
  let all_colors = all pi
  and between_colors = between pi in
  (* XXX *)
  all_colors <= x
  && match b, CS.cardinal between_colors with
    | Some b', 1 -> CS.choose between_colors = b'
    | Some _, 0
    | None, 0 -> true
    | _, _ -> false

(* As indicated by the underscore, this function is not designed to work as is.
 * Indeed, we need to preprocess with the case of c not being in any of the cut
 * sets under the internal node as defined in build_apartl below. *)
let _build_apartl cutsetl kappa (c, x) =
  let xopt = coptset_of_cset x in
  (* Anything in kappa - x doesn't get distributed. *)
  let big_b_excl =
    (* Because xopt never contains None, this is in fact testing c in x. In
     * this case, b will only be c, since c is added to big_b_excl later. *)
    if COS.mem c xopt then
      COS.empty
    else
      let to_exclude = coptset_of_cset (CS.diff kappa x) in
      COS.diff (coptset_of_cset (between cutsetl)) to_exclude
  in
  let to_distribute = COS.union xopt big_b_excl in
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
      and startsl = List.map (fun _ -> CS.empty) cutsetl in
      (* Finish off the meat of the recursion by mapping with union over the
       * cartesian product of the single-color distributions. *)
      let pis = List.rev
        (List.rev_map
           (transposed_fold CS.union startsl)
           (product dist))
      in
      (* We make sure to pass on the c as the color of the internal node in the
       * case where between pi is empty by filtering out the ones that don't. *)
      List.filter (fun pi -> not (CS.is_empty (between pi)) || b = c) pis)
    (* We add c to the list of things that can be colors of internal nodes. *)
    (COS.add c big_b_excl)
    []
  in
  apartl

let build_apartl_memo = Hashtbl.create 1024

(* The primary apartl builder.
 * Cutsetl is the list of cut sets below, kappa are those sets colors cut from
 * the internal node above. *)
let build_apartl cutsetl kappa (c, x) =
  (* If c is not in any of the cut sets below, then we can replace it with
   * None. *)
  let c = match c with
    | None -> None
    | Some c' -> if not (List.exists (CS.mem c') cutsetl) then None else c
  in
  let q = c, x in
  try
    Hashtbl.find build_apartl_memo (cutsetl, kappa, q)
  with
    | Not_found ->
      let ret = _build_apartl cutsetl kappa q in
      Hashtbl.add build_apartl_memo (cutsetl, kappa, q) ret;
      ret

let add_phi node question answer phi =
  let local_phi =
    try
      IntMap.find node phi
    with
      | Not_found -> QuestionMap.empty
  in
  IntMap.add node (QuestionMap.add question answer local_phi) phi

let null_apart = None, []

let rec phi_recurse cutsetim tree ((_, x) as question) phi =
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
        cutsetl
        (IntMap.find i cutsetim)
        question
      in
      (* Recur over subtrees to calculate (omega, updated_phi) for the apart
       * (b, pi). *)
      let apart_omega phi (b, pi) =
        List.fold_left2
          (fun (phi, subtotal) pi_i subtree ->
            let phi, omega = phi_recurse cutsetim subtree (b, pi_i) phi in
            phi, subtotal + omega)
          (phi, 0)
          pi
          subtrees
      in
      (* My understanding is that this None/Some below is just to start the
       * folding going. *)
      List.fold_left
        (fun (phi, cur) apart ->
          let phi, omega = apart_omega phi apart in
          match cur with
            | None -> phi, Some (omega, apart)
            | Some (old_omega, _) when omega > old_omega -> phi, Some (omega, apart)
            | _ -> phi, cur)
        (phi, None)
        apartl
  in
  match res with
    | Some (omega, apart) ->
      let phi' = add_phi i question (apart, omega) phi in
      phi', omega
    | None -> phi, 0

(* XXX Do you really need to recur over the tree here? It seems to me that
 * everything you need is in the cutsetim. *)
let badness cutsetim tree =
  let badness_i i = max 0 ((CS.cardinal (IntMap.find i cutsetim)) - 1) in
  let rec aux worst total = function
    | Leaf i :: rest ->
      let b = badness_i i in
      aux (max worst b) (total + b) rest
    | Node (i, subtrees) :: rest ->
      let b = badness_i i in
      aux (max worst b) (total + b) (List.rev_append subtrees rest)
    | [] -> worst, total
  in
  aux 0 0 [tree]

let solve ((_, tree) as cdtree) =
  let _, cutsetim = build_sizemim_and_cutsetim cdtree in
  let cutsetim = IntMap.add (top_id tree) CS.empty cutsetim in
  Hashtbl.clear build_apartl_memo;
  phi_recurse cutsetim tree (None, CS.empty) IntMap.empty

(* Given a phi (an implicit solution) get an actual solution, i.e. a subset of
 * the leaves to include. The recursion works as follows: maintain rest, which
 * is the list of things that remain to be expanded. If the first element of
 * rest has a leaf, then add it to the list if appropriate and recur. If not,
 * expand out the subtrees of the first element of rest and recur.
 * *)
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
      let (b, pi), _ = QuestionMap.find question (IntMap.find i phi) in
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
let name_map_of_bark_map bark_map =
  IntMap.fold
    (fun i bark accum ->
      try
        StringMap.add bark#get_name i accum
      with
        | Newick_bark.No_name -> accum)
    bark_map
    StringMap.empty

let rank_color_map_of_refpkg rp =
  let gt = Refpkg.get_ref_tree rp in
  let node_map = name_map_of_bark_map gt.Gtree.bark_map in
  let td = Refpkg.get_taxonomy rp
  and seqinfo = Refpkg.get_seqinfom rp in
  let add_to_rankmap seq rankmap ti =
    match begin
      try
        Some (StringMap.find seq node_map)
      with
        | Not_found -> None
    end with
      | Some node ->
        let rank = Tax_taxonomy.get_tax_rank td ti in
        let seqmap =
          try
            IntMap.find rank rankmap
          with
            | Not_found -> IntMap.empty
        in
        IntMap.add
          rank
          (IntMap.add node (Tax_taxonomy.get_tax_name td ti) seqmap)
          rankmap
      | None -> rankmap
  in
  StringMap.fold
    (fun seq {Tax_seqinfo.tax_id = ti} rankmap ->
      List.fold_left
        (add_to_rankmap seq)
        rankmap
        (Tax_taxonomy.get_lineage td ti))
    seqinfo
    IntMap.empty
