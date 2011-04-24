
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


(* testing *)

let sigma = auto_list_split [2;5;6];;
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


