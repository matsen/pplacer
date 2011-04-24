
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

auto_list_split [2;5;6];;

