(* assumes that the bark is at least a newick bark
*)

open MapsSets

let union = IntMapFuns.union

let ppr ff bm =
  IntMapFuns.ppr_gen (fun ff bark -> bark#ppr ff) ff bm

let boost by m =
  IntMap.fold (fun k v -> IntMap.add (k+by) v) m IntMap.empty

let get_bl m id = (IntMap.find id m)#get_bl
let get_name m id = (IntMap.find id m)#get_name
let get_boot m id = (IntMap.find id m)#get_boot

let to_name_map bm =
  IntMap.fold
    (fun id bark accu ->
      try
        IntMap.add id bark#get_name accu
      with
      | Newick_bark.No_name -> accu)
    bm
    IntMap.empty

