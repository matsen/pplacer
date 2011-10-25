(* assumes that the bark is at least a newick bark
*)

open Ppatteries

let union = IntMap.union

let ppr ff bm =
  IntMap.ppr_gen (fun ff bark -> bark#ppr ff) ff bm

let boost by m =
  IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty

let get_bl m id = (IntMap.find id m)#get_bl
let get_node_label m id = (IntMap.find id m)#get_node_label
let get_edge_label m id = (IntMap.find id m)#get_edge_label

let to_node_label_map bm =
  IntMap.fold
    (fun id bark accu ->
      try
        IntMap.add id bark#get_node_label accu
      with
      | Newick_bark.No_node_label -> accu)
    bm
    IntMap.empty

