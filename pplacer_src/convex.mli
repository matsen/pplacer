
type cset = color set
type csetl = color set list
type apart_t = color option * csetl (* almost partition *)
type sizem = int ColorMap.t
type colorm = color IntMap.t
type question = int * color option * cset (* a pair (i, c, X) *)

val is_apart: apart_t -> bool
(** Test if an apart_t is indeed an apart. *)

val add_elt_to_apart: xl:csetl -> start:apart -> color -> apart list
(** add_elt xl start c gives all of the aparts for xl that can be made from
 * start by adding color to start. *)

val build_sizem: stree -> colorm -> sizem IntMap.t
(** Given a colored tree, for every (integer-indexed) node record the number of
 * leaves below with a given color. *)

val single_nu: cset -> sizem -> int
(** single_nu cset sizem simply totals up the entries of sizem for colors in
 * cset. That is, it gives the number of leaves that are below with colors in
 * cset. *)

val list_nu: csetl -> sizem -> int
(** list_nu csetl sizem simply totals up the entries of sizem for colors in cset.
 * That is, it gives the number of leaves that are below with colors in cset. *)


(* the routine:

preparations
------------
- run build_sizem.


the potentially global state
----------------------------
phi, a question to apart_t map (this is the actual solution map)

Do we also want to keep around the set of Deltas that we have constructed?
That way, we can do stepwise addition.

- we are not going to be phi-ing every X, but we are going to be constructing
every element of Delta_i(c,X). for a given c and X.
- this way stepwise addition will be super useful, and we only have to do it
locally.
- we could try to keep some of these around but I don't think that would
actually help anything-- just get confusing.


recursion
---------

- would like to come up with \Phi_i(c,X).
- make Delta_i(c,X)
- rank these in terms of list_nu
- go down the list, for each (b, X_k) finding the size of \Phi_k(b, X_k),
keeping track of the biggest size
- when list_nu gets below this size then stop.

*)
