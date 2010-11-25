
open MapsSets

let ct_fname = "/home/bvdiversity/working/matsen/complete/cluster/phy/cluster.tre"
let boot_fname = "/home/bvdiversity/working/matsen/complete/cluster/boot/all.tre"

module StringSetSet = 
  Set.Make(struct type t = StringSet.t let compare = StringSet.compare end)

let sss_of_tree t = 
  IntMap.fold 
    (fun _ s -> StringSetSet.add s) 
    (Clusterfind.ssim_of_tree t)
    StringSetSet.empty

let ct = Newick.of_file ct_fname
let ssim = Clusterfind.ssim_of_tree ct  
let boot_tl = Newick.list_of_file boot_fname
let boot_sssl = List.map sss_of_tree boot_tl

let taxon_list t = List.map (Gtree.get_name t) (Gtree.leaf_ids t)
let taxon_set t = List.fold_right StringSet.add (taxon_list t) StringSet.empty

let taxs = taxon_set ct
let boot_ssl = List.map taxon_set boot_tl

let () = 
  List.iter 
    (fun ss -> if 0 <> StringSet.compare taxs ss then 
                 invalid_arg "taxon sets not identical")
    boot_ssl
    
let count_mem ss = 
  List.fold_right
    (fun bsss -> ( + ) (if StringSetSet.mem ss bsss then 1 else 0))
    boot_sssl
    0

let bootval_im = IntMap.map count_mem ssim

let boot_decorated = 
  Gtree.set_bark_map
    ct
    (IntMap.mapi 
      (fun i b ->
        match b#get_name_opt with
        | Some _ -> b
        | None ->
            (let cnode_num = int_of_float b#get_boot in
              (b#set_name (string_of_int cnode_num))
                #set_boot_opt 
                  (let boot_val = IntMap.find cnode_num bootval_im in
                  if boot_val >= 80 then Some (float_of_int boot_val) 
                  else None)))
      (Gtree.get_bark_map ct))

let () = Phyloxml.tree_to_file boot_decorated "cluster_boot.xml" 
