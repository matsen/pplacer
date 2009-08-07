
(*
 * prepare_from_lv_list_list:
 * turn lv's into glv's then combine over rates and sites
 *)
let prepare_from_lv_map_list_list locs lv_map_list_list = 
  combine_maps Glv.combine_over_rates locs (
    combine_maps Glv.combine_over_sites locs (
      List.map 
        (IntMap.map (Glv.of_like_vect)) 
        lv_map_list))

(*
 * mask_and_prepare:
 * mask then combine over rates and sites
 *)
let mask_and_prepare locs mask_arr lv_map_array_list = 
  prepare_from_lv_map_list_list locs (
    List.map 
      (fun single_rate -> mask mask_arr single_rate)
      lv_map_array_list)

