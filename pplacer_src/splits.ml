let rng = Gsl_rng.make (Gsl_rng.default ())

open MapsSets

module Lset = IntSet
module LsetFuns = IntSetFuns

module OrderedLset = struct
  type t = Lset.t
  let compare = Lset.compare
end

module PprLset = struct
  type t = Lset.t
  let ppr = LsetFuns.ppr
end

module Lsetset = Set.Make(OrderedLset)
module LsetsetFuns = SetFuns (OrderedLset) (PprLset)


type split = Lset.t * Lset.t

let sort_split p =
  let (a,b) = p in
  if 0 > Lset.compare a b then p else (b,a)

let make_split a b = sort_split (a,b)

module OrderedSplit = struct
  type t = split
  let compare (a1, a2) (b1, b2) =
    match Lset.compare a1 b1 with
      | 0 -> Lset.compare a2 b2
      | x -> x
end

module PprSplit = struct
  type t = split
  let ppr ff (a, b) =
    Format.fprintf ff "@[(";
    LsetFuns.ppr ff a;
    Format.fprintf ff ",@ ";
    LsetFuns.ppr ff b;
    Format.fprintf ff ")@]";
end

module Sset = Set.Make (OrderedSplit)
module SsetFuns = SetFuns (OrderedSplit) (PprSplit)

let split_lset split lset =
  LsetsetFuns.of_list (
    List.filter (fun s -> not (Lset.is_empty s))
      [
        Lset.inter (fst split) lset;
        Lset.inter (snd split) lset;
      ]
  )

let split_lsetset split lsetset =
  Lsetset.fold
    (fun ls lss -> Lsetset.union lss (split_lset split ls))
    lsetset
    Lsetset.empty

let sset_lsetset = Sset.fold split_lsetset

(* lacking SPEED. *)
let split_does_cut_lset split lset =
  1 <> Lsetset.cardinal (split_lset split lset)


(* lacking SPEED. *)
let split_does_cut_lsetset split lsetset =
  Lsetset.cardinal lsetset <> Lsetset.cardinal (split_lsetset split lsetset)

let select_sset_cutting_lsetset splits leafss =
  Sset.filter
    (fun split -> split_does_cut_lsetset split leafss)
    splits

(* Throw n_items into n_bins such that each bin is not empty. *)
let nonempty_balls_in_boxes rng ~n_bins ~n_items =
  assert(n_items >= n_bins);
  let counts = Array.create n_bins 1 in
  for i=1 to n_items - n_bins do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts

(*nonempty_balls_in_boxes rng ~n_bins:5 ~n_items:10*)

let lsetset_to_array lss =
  let a = Array.make (Lsetset.cardinal lss) (Lsetset.choose lss) in
  let _ = Lsetset.fold (fun x i -> a.(i) <- x; i+1) lss 0 in
  a

(* Throw the elements of lss into n_bins boxes, ensuring that each box has at
 * least one element.
 * The implementation below could be made more functional and list-y but I don't
 * think that it would be shorter.
 * *)
let uniform_nonempty_partition rng n_bins lss =
  let a = lsetset_to_array lss in
  (* XXX *)
  Guppy_kr.shuffle rng a;
  let pos = ref 0 in
  List.map
    (fun n_samples ->
      let new_lss =
        LsetsetFuns.of_list (Array.to_list (Array.sub a !pos n_samples)) in
      pos := !pos + n_samples;
      new_lss)
    (nonempty_balls_in_boxes rng ~n_bins ~n_items:(Lsetset.cardinal lss))


module G = Gsl_rng
module GD = Gsl_randist

let uniform_sample rng n k =
  if k > n then failwith "k > n";
  let rec aux accum = function
    | 0 -> accum
    | k ->
      let rec select () =
        match Gsl_rng.uniform_int rng n with
          | i when List.mem i accum -> select ()
          | i -> i
      in
      aux ((select ()) :: accum) (k - 1)
  in
  aux [] k

let repeat f n =
  let rec aux accum = function
    | 0 -> accum
    | n -> aux ((f n) :: accum) (n - 1)
  in
  aux [] n

let rev_enumerate ?(start = 0) l =
  let _, l' = List.fold_left
    (fun (e, accum) x -> e + 1, (e, x) :: accum)
    (start, [])
    l
  in
  l'

let generate_yule rng count =
  let rec aux next_id trees =
    match List.length trees with
      | 1 -> List.hd trees
      | n ->
        let nodes = uniform_sample rng n 2 in
        let _, trees', nodes' = List.fold_left
          (fun (e, l1, l2) x ->
            if List.mem e nodes then
              e + 1, l1, x :: l2
            else
              e + 1, x :: l1, l2)
          (0, [], [])
          trees
        in
        aux
          (next_id + 1)
          (Stree.node next_id nodes' :: trees')
  in
  aux count (repeat Stree.leaf count)

let get_lset =
  let rec aux accum = function
    | Stree.Leaf n -> Lset.add n accum
    | Stree.Node (_, subtree) -> List.fold_left aux accum subtree
  in
  aux Lset.empty

let get_sset =
  let rec aux accum above = function
    | Stree.Leaf n -> Sset.add (make_split above (Lset.singleton n)) accum
    | Stree.Node (_, subtree) ->
      List.fold_left
        (fun accum tree ->
          let above =
            List.fold_left
              (fun lset tree' ->
                if tree' = tree then
                  lset
                else
                  Lset.union lset (get_lset tree'))
              above
              subtree
          in
          let ret = aux accum above tree in
          if Lset.is_empty above then
            ret
          else
            Sset.add
              (make_split above (get_lset tree))
              ret)
        accum
        subtree
  in
  aux Sset.empty Lset.empty

let generate_root rng include_prob poisson_mean ?(min_leafs = 0) splits leafs =
  let k =
    max
      (min_leafs - 1)
      (Gsl_randist.poisson rng poisson_mean)
  in
  let splits' = SsetFuns.uniform_sample (uniform_sample rng) splits k in
  let leafs' = sset_lsetset splits' (Lsetset.singleton leafs) in
  let base_leafs =
    LsetsetFuns.uniform_sample
      (uniform_sample rng)
      leafs'
      min_leafs
  in
  let to_sample =
    Lsetset.filter
      (fun lset -> not (Lsetset.mem lset base_leafs))
      leafs'
  in
  Lsetset.union
    base_leafs
    (Lsetset.filter
       (fun _ -> Gsl_rng.uniform rng < include_prob)
       to_sample)

let main rng include_prob poisson_mean yule_size tree =
  let leafss =
    generate_root
      rng
      include_prob
      poisson_mean
      ~min_leafs:yule_size
      (get_sset tree)
      (get_lset tree)
  in
  let yule_tree = generate_yule rng yule_size in
  let partitioned = uniform_nonempty_partition rng yule_size leafss in
  IntMapFuns.of_pairlist (rev_enumerate ~start:1 partitioned),
  yule_tree

(* convenience *)

let rec range n =
  if n > 0 then n::(range (n-1))
  else []

let n_set n = LsetFuns.of_list (range n)

let outer_inner_split outer inner =
  assert(Lset.subset inner outer);
  make_split inner (Lset.diff outer inner)


let auto_split ls =
  let n = 1 + Lset.max_elt ls in
  outer_inner_split (n_set n) ls


let auto_list_split l =
  auto_split (LsetFuns.of_list l)


(* testing *)

let sigma = auto_list_split [2;5;6]
let sigma' = auto_list_split [6]
let ls1 = LsetFuns.of_list [1;2]
let ls2 = LsetFuns.of_list [3;5;6]
let ls3 = LsetFuns.of_list [2;5]
let lss = LsetsetFuns.of_list [ls1; ls2]
let cut_lss = split_lsetset sigma lss

(* split_lset sigma ls1 *)

(* split_does_cut_lset sigma ls2 *)
(* split_does_cut_lset sigma ls3 *)
(* split_does_cut_lsetset sigma lss *)
(* split_does_cut_lsetset sigma cut_lss *)

(* lsetset_to_array lss *)

(* uniform_nonempty_partition rng 2 cut_lss *)

