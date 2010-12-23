  (*
   * insert in find just before get_mass_list.
  let print_edge_info id =
    let our_mass_list = 
      if IntMap.mem id smass then IntMap.find id smass
      else []
    and bl = Gtree.get_bl ref_tree id in
    Printf.printf "edge id: %d\n" id;
    Printf.printf "prox: %g\n" (delta ~prox_ml:[] ~dist_ml:our_mass_list id bl);
    Printf.printf "dist: %g\n" (delta ~prox_ml:our_mass_list ~dist_ml:[] id 0.);
  in
  *)

