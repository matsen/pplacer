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

let build_sizem_and_csetm (colors, tree) =
  let rec aux = function
    | Leaf i ->
      let color = IntMap.find i colors in
      let szm = ColorMap.singleton color 1
      and clm = ColorSet.singleton color in
      szm, clm, IntMap.singleton i (szm, clm)
    | Node (i, subtrees) ->
      let maps = List.map aux subtrees in
      let szm = ColorMap.merge_counts (List.map (fun (a, _, _) -> a) maps) in
      let clm, leafm = List.fold_left
        (fun (claccum, lfaccum) (_, cl, lf) ->
          ColorSet.union claccum cl, IntMap.union lfaccum lf)
        (ColorSet.empty, IntMap.empty)
        maps
      in
      szm, clm, IntMap.add i (szm, clm) leafm
  in
  let _, _, leafm = aux tree in
  let clm = IntMap.map snd leafm
  and leafm' = IntMap.map fst leafm in
  let rec aux unterminated accum = function
    | Leaf _ -> accum
    | Node (_, subtrees) ->
      let colorsets = List.map
        (function
          | Leaf i
          | Node (i, _) -> IntMap.find i accum)
        subtrees
      in
      let big_b = ColorSet.union unterminated (between colorsets) in
      List.fold_left2
        (fun accum colors tree ->
          let i = match tree with
            | Leaf i
            | Node (i, _) -> i
          in
          let colors' = ColorSet.inter colors big_b in
          if colors = colors' then
            accum
          else
            aux
              big_b
              (IntMap.add i colors' accum)
              tree)
        accum
        colorsets
        subtrees
  in
  let clm = aux ColorSet.empty clm tree in
  leafm', clm

let rec powerset = function
  | [] -> [[]]
  | _ :: t as l -> List.fold_left (fun xs t -> l :: t :: xs) [] (powerset t)

let coptset_of_cset cset =
  ColorSet.fold (fun c s -> ColorOptSet.add (Some c) s) cset ColorOptSet.empty

let build_apartl csetl (c, x) =
  let big_b = ColorOptSet.add c (coptset_of_cset (between csetl)) in
  let apartl = ColorOptSet.fold
    (fun b accum ->
      let to_split = ColorOptSet.remove b big_b in
      let rec aux used prospect accum = function
        | [] -> (List.rev prospect) :: accum
        | cset :: rest ->
          let cset = ColorSet.inter cset x in
          (* Simple colors are the ones which aren't in B \ {b}. *)
          let cs_unsimple, cs_simple = ColorSet.partition
            (fun cs -> ColorOptSet.mem (Some cs) to_split)
            cset
          in
          let cs_simple' = ColorSet.union cs_simple used in
          let choices = powerset (ColorSet.elements cs_unsimple) in
          List.fold_left
            (fun accum choice ->
              let choice = ColorSet.of_list choice in

              (* Check if there's any colors in this choice which have already
               * been used by other previous color sets. *)
              if not (ColorSet.is_disjoint choice used)
                (* Or, if b != c, make sure that c isn't in this choice. *)
                || begin
                  match c with
                    | Some c' when b != c -> ColorSet.mem c' choice
                    | _ -> false
                end
              then
                accum

              else
                aux
                  (ColorSet.union cs_simple' choice)
                  ((ColorSet.union cs_simple choice) :: prospect)
                  accum
                  rest)
            accum
            choices
      in
      let pi = aux ColorSet.empty [] [] csetl in
      (b, pi) :: accum)
    big_b
    []
  in
  List.fold_left
    (fun accum (b, csetll) ->
      List.rev_append
        (List.map
           (fun csetl -> b, csetl)
           csetll)
        accum)
    []
    apartl
