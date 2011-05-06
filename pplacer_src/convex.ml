open MapsSets
open Stree

type color = string
module ColorSet = StringSet
module ColorMap = StringMap
type cset = ColorSet.t
type 'a cmap = 'a ColorMap.t

module OrderedCset = struct
  type t = cset
  let compare = ColorSet.compare
end
module QuestionMap = Map.Make(OrderedCset)
type 'a qmap = 'a QuestionMap.t

type question = color option * cset (* a pair (c, X) *)


type csetl = ColorSet.t list
type apart = color option * csetl  (* apart = almost partition *)
type sizem = int ColorMap.t
type colorm = color IntMap.t
type cdtree = colorm * stree
type local_phi = apart QuestionMap.t
type phi = local_phi IntMap.t

