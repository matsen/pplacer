
#install_printer Panda_core.ppr_kr_info

let (ref_tree1,npcl1) = Placement_io.parse_place_file "v0.2" "test1.place"
let (ref_tree2,npcl2) = Placement_io.parse_place_file "v0.2" "test2.place"
let () = assert(ref_tree1 = ref_tree2)

let y = Panda_core.pair_core Placement.compare_ml_place false ref_tree1
("one",npcl1) ("two",npcl2)
