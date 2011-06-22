open MapsSets
open Stree
type color = string

module ColorSet: MapsSets.S with type elt = color
module ColorMap: MapsSets.M with type key = color
type cset = ColorSet.t
type 'a cmap = 'a ColorMap.t

type question = color option * cset (* a pair (c, X) *)
module QuestionMap: MapsSets.M with type key = question
type 'a qmap = 'a QuestionMap.t

type csetl = cset list
type apart = color option * csetl  (* apart = almost partition *)
type sizem = int cmap
type colorm = color IntMap.t
type cdtree = colorm * stree
type local_phi = (apart * int) qmap
type phi = local_phi IntMap.t
(* Here the int list is the list of top_id's that are in parallel with the
 * top_id's of the subtree below. *)
type nu_f = phi -> apart -> int list -> int

(* QuestionMap should be a map from questions *)

(* Basics. *)

val all: csetl -> cset
(** Just list union, called A in the manuscript. *)

val between: csetl -> cset
(** The union of the pairwise intersections, called B in the manuscript. *)


(* Getting ready. *)

val build_sizemim_and_cutsetim: cdtree -> sizem IntMap.t * cset IntMap.t
(** Given a colored tree, for every (integer-indexed) node record the number of
 * leaves below with a given color.
 *
 * Make a map that goes from every internal node to the color sets in the
 * subtrees below that internal node. *)


(* Building up aparts. *)

val build_apartl: csetl -> cset -> question -> apart list
(** Given an [X1,...,Xk] and a (c, X) return a list of (b, pi)'s.
 * If c is in X or if B = B([X1,...,Xk]) is empty, then b = c.
 * Otherwise, b can be anything in B.
 * In either case, B(pi) must be a subset of {b} and A(pi) = X.
 * Build up stepwise using add_elt_to_apart, starting with the list consisting
 * of all (b,pi) such that pi is just the possible distributions of b into the
 * x's.
 * *)


(* For the recursion. *)

(* val single_naive_upper: chosen:cset -> cutset:cset -> sizem -> int *)
(* (\** naive_upper chosent cutset sizem gives an naive (i.e. ignoring convexity) *)
(*  * upper bound for the number of leaves below that could be allowed if we select *)
(*  * the chosen subset of the cutset. *\) *)

val apart_nu: cset -> sizem list -> apart -> int
(** convenience function for running list_nu on the csetl of an apart. *)


(* The recursion, as it were. *)

val phi_recurse: cset IntMap.t -> sizem list IntMap.t -> Stree.stree -> question -> phi -> phi * int
(** phi_recurse id q phi returns a phi map which includes the answer
 * to the posed question.
 *)

(* Version with lower bounds.
 * First make sure that the question isn't already answered in phi.
 * If so, return phi.
 * Otherwise...

 let csetl = IntMap.find node_num cutsetlm
 let apartl = build_apartl csetl q in
 let nul = List.map apart_nu apartl in

 ... sort the apartl in decreasing order according to nul ...

 proceed down the sorted apartl and nul lists.

 inner recursion:

   keep track of the best solution found so far (best_phi) and its size
   (best_size).

   pick off the next apart and nu. if none then return (best_phi, best_size)
   if nu is less than or equal to best_nu then return (best_phi, best_size)
   otherwise if apart is (b, pi) then for each x_i pi, then recur down the
   subtree i with the question (b, x_i).
   take that phi and pass on to the next subtree.

   If it's a leaf, then just return the same phi and 0 if the incoming question
   is not empty and one otherwise (assert that it doesn't have more than one
   elt.)

*)

val solve: cdtree -> phi * int

val badness: cset IntMap.t -> int * int
val nodeset_of_phi_and_tree: phi -> stree -> IntSet.t
val maplist_of_map_and_tree: 'a IntMap.t -> stree -> 'a list IntMap.t
val rank_color_map_of_refpkg: Refpkg.t -> color IntMap.t IntMap.t

val alternate_colors: cdtree -> cset IntMap.t
