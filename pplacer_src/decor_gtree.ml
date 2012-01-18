(* routines for gtrees which have a Decor bark map.
*)

open Ppatteries

type t = Decor_bark.decor_bark Gtree.gtree

let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2

let of_newick_gtree t = Gtree.map_bark_map Decor_bark.of_newick_bark t
let to_newick_gtree t = Gtree.map_bark_map Decor_bark.to_newick_bark t

let to_numbered t =
  Gtree.mapi_bark_map (fun i x -> x#to_numbered i) t

(* append (id, decorl) to the bark map *)
let map_add_decor_listly id decorl barkm =
  IntMap.add
    id
    ((if IntMap.mem id barkm then IntMap.find id barkm
    else new Decor_bark.decor_bark `Empty)
      #append_decor decorl)
    barkm

(* decor_map is an IntMap to a Decor.decoration list *)
let add_decor_by_map t decor_map =
  Gtree.set_bark_map t
    (IntMap.fold map_add_decor_listly decor_map (Gtree.get_bark_map t))

(* given a color, a tree and a set of leaves: first, give all of the leaves on
 * the tree that color. then, proceed up the tree giving all edges that same
 * color as long as all of the edges below also have that color. *)
let color_clades_above ?(color = Decor.red) leaves gt =
  let rec aux accum = function
    | Stree.Leaf i ->
      if IntSet.mem i leaves then
        map_add_decor_listly i [color] accum
      else
        accum
    | Stree.Node (i, subtrees) ->
      let accum' = List.fold_left aux accum subtrees in
      if List.for_all
        (fun t ->
          let n = Stree.top_id t in
          IntMap.mem n accum'
          && List.mem color (IntMap.find n accum')#get_decor)
        subtrees
      then
        map_add_decor_listly i [color] accum'
      else
        accum'
  in
  let decor_map' = aux (Gtree.get_bark_map gt) (Gtree.get_stree gt) in
  Gtree.set_bark_map gt decor_map'

(* not a real average, but good enough *)
let list_color_average = List.reduce (Decor.color_avg 0.5) |- some
let keep_only_one _ = None
let keep_first = function [x] -> Some x | _ -> None

let consolidate_colors resolution_fn gt =
  Gtree.get_bark_map gt
  |> IntMap.map
      (fun b ->
        let decor' = match List.partition
            (function Decor.Color _ -> true | _ -> false)
          b#get_decor
        with
          | [], others -> others
          | [color], others -> color :: others
          | colors, others -> maybe_cons (resolution_fn colors) others
        in
        b#set_decor decor')
  |> Gtree.set_bark_map gt
