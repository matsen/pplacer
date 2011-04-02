(*
"test1.place"
"test2.place"
"test3.place"
"test_all.place"
*)

open Clusterfunc
open MapsSets


let prl = List.map Placerun_io.of_file ["test1.place"; "test2.place"; "test3.place"]

let (rt, blobl) = t_named_prel_of_prl prl

open PreCluster

let bm_to_pairs m =
  let l = BMap.fold (fun k v l -> (k,v)::l) m [] in
  List.rev l

let (bmap, cset, barkm, free_index) = ingreds_of_named_blobl rt blobl

let x = bm_to_pairs bmap
let x = PreCluster.CSet.elements cset

let x = of_ingreds rt bmap cset barkm free_index

