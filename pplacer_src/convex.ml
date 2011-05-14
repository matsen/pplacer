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

type question = color option * cset (* a pair (c, X) *)

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
      | None, None ->
        ColorSet.compare cs1 cs2

      | None, Some _ -> -1
      | Some _, None -> 1
      | Some c1, Some c2 -> String.compare c1 c2
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

module XXX = Refpkg

let all colors = List.fold_left ColorSet.union ColorSet.empty colors
let between colors = all
  (List.map
     (fun (x, y) -> ColorSet.inter x y)
     (Base.list_pairs_of_single colors))

let build_sizemim_and_cutsetim (colors, tree) =
  (* Building an internal_node -> szm, color_below map. *)
  let rec aux = function
    | Leaf i ->
      let color = IntMap.find i colors in
      let szm = ColorMap.singleton color 1
      and clbelow = ColorSet.singleton color in
      szm, clbelow, IntMap.singleton i (szm, clbelow)
    | Node (i, subtrees) ->
      let maps = List.map aux subtrees in
      let szm = ColorMap.merge_counts (List.map (fun (a, _, _) -> a) maps) in
      let clbelow, leafm = List.fold_left
        (fun (claccum, lfaccum) (_, cl, lf) ->
          ColorSet.union claccum cl, IntMap.union lfaccum lf)
        (ColorSet.empty, IntMap.empty)
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
      let terminated' = ColorSet.union terminated (between colorsets) in
      List.fold_left2
        (fun accum colors tree ->
          let i = top_id tree in
          (* colors' are just those edge colors in terminated' *)
          let colors' = ColorSet.inter colors terminated' in
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
  let cut_clm = aux ColorSet.empty below_clm tree in
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

(* Find the potential distributions of a color across a list of cut sets. *)
let cutsetdist cutsetl color =
  let rec aux base accum = function
    | [] -> List.map List.rev accum
    | cutset :: rest ->
      let accum = List.map (fun x -> ColorSet.empty :: x) accum in
      let accum =
        if ColorSet.mem color cutset then
          (ColorSet.singleton color :: base) :: accum
        else
          accum
      in
      aux (ColorSet.empty :: base) accum rest
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
  ColorSet.fold (fun c s -> ColorOptSet.add (Some c) s) cset ColorOptSet.empty

let cset_of_coptset coptset =
  ColorOptSet.fold
    (fun c s ->
      match c with
        | Some c' -> ColorSet.add c' s
        | None -> s)
    coptset
    ColorSet.empty

let is_apart (b, pi) x =
  let all_colors = all pi
  and between_colors = between pi in
  all_colors <= x
  && match b, ColorSet.cardinal between_colors with
    | Some b', 1 -> ColorSet.choose between_colors = b'
    | Some _, 0
    | None, 0 -> true
    | _, _ -> false

let build_apartl cutsetl kappa (c, x) =
  let x' = coptset_of_cset x in
  let to_cut = coptset_of_cset (ColorSet.diff kappa x) in
  let c_in_x = ColorOptSet.mem c x' in
  let check_pi b pi =
    if c_in_x || ColorSet.is_empty (between pi) then
      b = c
    else
      true
  in
  let big_b = ColorOptSet.add c (ColorOptSet.diff (coptset_of_cset (between cutsetl)) to_cut) in
  let apartl = ColorOptSet.fold
    (fun b accum ->
      let to_distribute = ColorSet.union
        x
        (cset_of_coptset (ColorOptSet.diff (ColorOptSet.remove b big_b) to_cut))
      in
      let dist = List.map
        (cutsetdist cutsetl)
        (ColorSet.elements to_distribute)
      in
      let prod = product dist in
      let starts = List.map
        begin match b with
          | Some b' -> ColorSet.inter (ColorSet.singleton b')
          | None -> fun _ -> ColorSet.empty
        end
        cutsetl
      in
      let pis =
        List.map
          (transposed_fold ColorSet.union starts)
          prod
      in
      (* Unpack from a color set list list to an apart list. *)
      List.fold_left
        (fun accum pi -> if check_pi b pi then (b, pi) :: accum else accum)
        accum
        pis)
    big_b
    []
  in
  apartl

let single_nu cset sizem =
  ColorSet.fold
    (fun color accum ->
      let size =
        try
          ColorMap.find color sizem
        with
          | Not_found -> 0
      in
      size + accum)
    cset
    0
let list_nu csetl sizeml =
  List.fold_left2
    (fun accum cset sizem -> (single_nu cset sizem) + accum)
    0
    csetl
    sizeml

let apart_nu (_, csetl) sizeml = list_nu csetl sizeml


let add_phi node question answer phi =
  let local_phi =
    try
      IntMap.find node phi
    with
      | Not_found -> QuestionMap.empty
  in
  let local_phi' = QuestionMap.add question answer local_phi in
  IntMap.add node local_phi' phi

let null_apart = None, []

let rec phi_recurse cutsetm tree ((_, x) as question) phi =
  let i = top_id tree in
  match begin
    try
      Some (QuestionMap.find question (IntMap.find i phi))
    with
      | Not_found -> None
  end with
    | Some (_, nu) -> phi, nu
    | None ->

  let phi, apart, nu = match tree with
    | Leaf _ ->
      let nu = if x = IntMap.find i cutsetm then 1 else 0 in
      phi, null_apart, nu
    | Node (_, subtrees) ->
      let cutsetl = List.map
        (fun subtree -> IntMap.find (top_id subtree) cutsetm)
        subtrees
      in
      let apartl = build_apartl cutsetl (IntMap.find i cutsetm) question in
      let apart_nu phi (c, csetl) =
        List.fold_left2
          (fun (phi, cur) cset subtree ->
            let phi, nu = phi_recurse cutsetm subtree (c, cset) phi in
            phi, cur + nu)
          (phi, 0)
          csetl
          subtrees
      in
      let phi, res =
        List.fold_left
          (fun (phi, cur) apart ->
            let phi, nu = apart_nu phi apart in
            match cur with
              | None -> phi, Some (nu, apart)
              | Some (old_nu, _) when nu > old_nu -> phi, Some (nu, apart)
              | _ -> phi, cur)
          (phi, None)
          apartl
      in
      let nu, apart = match res with
        | Some t -> t
        | None -> failwith "no apartl?"
      in
      phi, apart, nu
  in
  let phi' = add_phi i question (apart, nu) phi in
  phi', nu


let solve ((_, tree) as cdtree) =
  let _, cutsetm = build_sizemim_and_cutsetim cdtree in
  let cutsetm = IntMap.add (top_id tree) ColorSet.empty cutsetm in
  phi_recurse cutsetm tree (None, ColorSet.empty) IntMap.empty

let nodeset_of_phi_and_tree phi tree =
  let rec aux accum = function
    | (Leaf i, question) :: rest ->
      let _, nu = QuestionMap.find question (IntMap.find i phi) in
      let accum =
        if nu = 0 then
          accum
        else
          IntSet.add i accum
      in
      aux accum rest
    | (Node (i, subtrees), question) :: rest ->
      let (c, csetl), _ = QuestionMap.find question (IntMap.find i phi) in
      let rest' = List.fold_left2
        (fun rest cset subtree -> (subtree, (c, cset)) :: rest)
        rest
        csetl
        subtrees
      in
      aux accum rest'
    | [] -> accum
  in
  aux IntSet.empty [tree, (None, ColorSet.empty)]
