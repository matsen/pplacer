(* routines for gtrees which have a Decor bark map.
*)

open Fam_batteries
open MapsSets

type t = Decor_bark.decor_bark Gtree.gtree

let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2

let of_newick_gtree t = Gtree.map_bark_map Decor_bark.of_newick_bark t
let to_newick_gtree t = Gtree.map_bark_map Decor_bark.to_newick_bark t

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
