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
type local_phi = apart QuestionMap.t
type phi = local_phi IntMap.t

module XXX = Newick_gtree

let all colors = List.fold_left ColorSet.union ColorSet.empty colors
let between colors = all
  (List.map
     (fun (x, y) -> ColorSet.inter x y)
     (Base.list_pairs_of_single colors))

let build_sizem_and_cutsetm (colors, tree) =
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
  and clm = IntMap.map snd leafm
  in
  (* Refines the clm to just map to the cut colors.
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
        (function
          | Leaf i
          | Node (i, _) -> IntMap.find i accum)
        subtrees
      in
      (* Update terminated. *)
      let terminated' = ColorSet.union terminated (between colorsets) in
      List.fold_left2
        (fun accum colors tree ->
          let i = match tree with
            | Leaf i
            | Node (i, _) -> i
          in
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
  let clm = aux ColorSet.empty clm tree in
  szm, clm

let cutsetlm_of_cutsetm_and_tree cutsetm tree =
  let rec aux accum = function
    | [] -> accum
    | Leaf i :: rest ->
      aux
        (IntMap.add i [] accum)
        rest
    | Node (i, subtrees) :: rest ->
      aux
        (IntMap.add
           i
           (List.map
              (function
                | Leaf i
                | Node (i, _) -> IntMap.find i cutsetm)
              subtrees)
           accum)
        (List.rev_append subtrees rest)
  in
  aux IntMap.empty [tree]

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

let is_apart (b, pi) x =
  let all_colors = all pi
  and between_colors = between pi in
  all_colors <= x
  && match b, ColorSet.cardinal between_colors with
    | Some b', 1 -> ColorSet.choose between_colors = b'
    | Some _, 0
    | None, 0 -> true
    | _, _ -> false

let build_apartl cutsetl (c, x) =
  let cutsetl = List.map (ColorSet.inter x) cutsetl in
  let big_b = ColorOptSet.add c (coptset_of_cset (between cutsetl)) in
  let apartl = ColorOptSet.fold
    (fun b accum ->
      let to_split = ColorOptSet.remove b big_b in
      let csl_unsimple, csl_simple = List.split
        (List.map
           (ColorSet.partition
              (fun color -> ColorOptSet.mem (Some color) to_split))
           cutsetl)
      in
      let q = all csl_unsimple in
      let dist = List.map (cutsetdist cutsetl) (ColorSet.elements q) in
      let prod = product dist in
      let pis =
        List.map
          (transposed_fold ColorSet.union csl_simple)
          prod
      in
      (* Unpack from a color set list list to an apart list. *)
      List.fold_left
        (fun accum pi -> (b, pi) :: accum)
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

let list_nu csetl sizem =
  List.fold_left
    (fun accum cset -> (single_nu cset sizem) + accum)
    0
    csetl

let apart_nu (_, csetl) sizem = list_nu csetl sizem
