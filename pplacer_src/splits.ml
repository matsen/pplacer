
open MapsSets;;

module Lset = IntSet;;

module LsetFuns = IntSetFuns;;

module OrderedLset = struct
  type t = Lset.t
  let compare = Lset.compare
end
  ;;

module PprLset = struct
  type t = Lset.t
  let ppr = LsetFuns.ppr
end
  ;;

module Lsetset = Set.Make(OrderedLset)
;;

module LsetsetFuns = SetFuns (OrderedLset) (PprLset)
;;

#install_printer LsetFuns.ppr;;
#install_printer LsetsetFuns.ppr;;

type split = Lset.t * Lset.t;;

let sort_split p =
  let (a,b) = p in
  if 0 > Lset.compare a b then p else (b,a)
;;

let make_split a b = sort_split (a,b)
;;

let split_lset split lset =
  LsetsetFuns.of_list (
    List.filter (fun s -> not (Lset.is_empty s))
      [
        Lset.inter (fst split) lset;
        Lset.inter (snd split) lset;
      ]
  )
;;

let split_lsetset split lsetset =
  Lsetset.fold
    (fun ls lss -> Lsetset.union lss (split_lset split ls))
    lsetset
    Lsetset.empty
;;

(* lacking SPEED. *)
let split_does_cut_lset split lset =
  1 <> Lsetset.cardinal (split_lset split lset)
;;

(* lacking SPEED. *)
let split_does_cut_lsetset split lsetset =
  Lsetset.cardinal lsetset <> Lsetset.cardinal (split_lsetset split lsetset)
;;

(* Throw n_items into n_bins such that each bin is not empty. *)
let nonempty_balls_in_boxes rng ~n_bins ~n_items =
  assert(n_items >= n_bins);
  let counts = Array.create n_bins 1 in
  for i=1 to n_items - n_bins do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts
;;
nonempty_balls_in_boxes rng ~n_bins:5 ~n_items:10;;

let lsetset_to_array lss =
  let a = Array.make (Lsetset.cardinal lss) (Lsetset.choose lss) in
  let _ = Lsetset.fold (fun x i -> a.(i) <- x; i+1) lss 0 in
  a
;;

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
;;


(* convenience *)

let rec range n =
  if n > 0 then n::(range (n-1))
  else []
;;

let n_set n = LsetFuns.of_list (range n)
;;

let outer_inner_split outer inner =
  assert(Lset.subset inner outer);
  make_split inner (Lset.diff outer inner)
;;

let auto_split ls =
  let n = 1 + Lset.max_elt ls in
  outer_inner_split (n_set n) ls
;;

let auto_list_split l =
  auto_split (LsetFuns.of_list l)
;;


(* testing *)

let rng = Gsl_rng.make (Gsl_rng.default ());;

let sigma = auto_list_split [2;5;6];;
let sigma' = auto_list_split [6];;
let ls1 = LsetFuns.of_list [1;2];;
let ls2 = LsetFuns.of_list [3;5;6];;
let ls3 = LsetFuns.of_list [2;5];;
let lss = LsetsetFuns.of_list [ls1; ls2];;
split_lset sigma ls1;;
let cut_lss = split_lsetset sigma lss;;

split_does_cut_lset sigma ls2;;
split_does_cut_lset sigma ls3;;
split_does_cut_lsetset sigma lss;;
split_does_cut_lsetset sigma cut_lss;;

lsetset_to_array lss;;

uniform_nonempty_partition rng 2 cut_lss;;

