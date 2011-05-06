
type question = color option * cset (* a pair (c, X) *)

(* QuestionMap should be a map from questions *)

type cdtree = color IntMap.t * stree
type cset = ColorSet.t
type csetl = ColorSet.t list
type apart_t = color option * csetl  (* apart = almost partition *)
type sizem = int ColorMap.t
type colorm = color IntMap.t
type local_phi = apart_t QuestionMap.t
type phi = local_phi IntMap.t


(* Basics. *)

val all: csetl -> cset
(** Just list union, called A in the manuscript. *)

val between: csetl -> cset
(** The union of the pairwise intersections, called B in the manuscript. *)


(* Getting ready. *)

val build_sizem: stree -> colorm -> sizem IntMap.t
(** Given a colored tree, for every (integer-indexed) node record the number of
 * leaves below with a given color.
 *
 * NOTE: it may be easier, smaller, and faster to just have a integer matrix, indexed
 * first by the edge number and second by the color.
 *
 * This matrix will be sparse, but not outrageously so.
 *
 * *)

val build_csetlm: stree -> csetl IntMap.t
(** Make a map that goes from every internal node to the color sets in the
 * subtrees below that internal node. *)


(* Building up aparts. *)

val is_apart: apart_t -> bool
(** Test if an apart_t is indeed an apart. *)

val add_elt_to_apart: csetl -> apart -> color -> apart list
(** add_elt_to_apart xl start c gives all of the aparts for xl that
 * can be made from start by adding color to start. Assert that if (b,pi) =
 * start then b is not c.
 * Note that for performance it will be crucial that if a color only
 * appears in one element of the csetl that it gets added efficiently (this
 * should be easy). *)

val build_apartl: csetl -> question -> apart list
(** Given an [X1,...,Xk] and a (c, X) return a list of (b, pi)'s.
 * If c is in X or if B = B([X1,...,Xk]) is empty, then b = c.
 * Otherwise, b can be anything in B.
 * In either case, B(pi) must be a subset of {b} and A(pi) = X.
 * Build up stepwise using add_elt_to_apart, starting with the list consisting
 * of all (b,pi) such that pi is just the possible distributions of b into the
 * x's.
 * *)


(* For the recursion. *)

val single_nu: cset -> sizem -> int
(** single_nu cset sizem simply totals up the entries of sizem for colors in
 * cset. That is, it gives the number of leaves that are below with colors in
 * cset. *)

val list_nu: csetl -> sizem -> int
(** list_nu csetl sizem simply totals up the calues of single_nu applied to the
 * given csetl. *)

val apart_nu: apart_t -> sizem -> int
(** convenience function for running list_nu on the csetl of an apart_t. *)


(* The recursion, as it were. *)

val phi_recurse: int -> question -> phi -> phi * int
(** phi_recurse t node_num q phi returns a phi map which includes the answer
 * to the posed question.
 *
 * NOTE: this is not an independent function, but a closure inside solve that
 * has access to sizem and csetlm.
 *
 * First make sure that the question isn't already answered in phi.
 * If so, return phi.
 * Otherwise...

 let csetl = IntMap.find node_num csetlm
 let apartl = build_sizem csetl q in
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

*)

val solve: cdtree -> phi * int
(**

 let sizem = build_sizem yada yada
 let csetlm = build_csetlm yada yada

- run phi_recurse

*)
