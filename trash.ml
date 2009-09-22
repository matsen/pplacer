      (*
      List.iter
        (fun (loc, like) ->
          Printf.printf "%d\t%g\n" loc like) h_r;
      print_endline "";
      *)



GLV*******

(* take the total of f applied to sequential pairs from a and b *)
let apply_and_total2 f a b = 
  ArrayFuns.fold_left2 (fun sofar ae be -> sofar +. f ae be) 0. a b

(* log_like2_statd:
 * take the log like of the product of three things then dot with the stationary
 * distribution. there are lots of things we don't do error checking
 * on. *)
let log_like2_statd model x_glv y_glv = 
  assert(n_rates x_glv = n_rates y_glv);
  let fn_rates = float_of_int (n_rates x_glv) in
  let statd = model.statd in
  let size = Gsl_vector.length statd in
  finite_infinity 
    (ArrayFuns.fold_left2 (* fold over sites *)
      (fun site_tot x_site y_site -> 
        site_tot +. (* total is product in log world *)
          (log ((ArrayFuns.fold_left2 (* fold over rates *)
            (fun rate_tot x_lv y_lv -> 
              rate_tot+.(Base.triple_dot statd x_lv y_lv size))
            0. x_site y_site) /. fn_rates)))
      0. x_glv y_glv)


(* log_like2:
 * take the log like of the product of g1 and g2
 *)
let log_like2 g1 g2 = 
  assert(g1 <> [||]);
  assert(n_sites g1 = n_sites g2);
  let n_rates = Array.length g1.(0) in
  finite_infinity
    (apply_and_total2
      (fun site_g1 site_g2 ->
        (log 
          (apply_and_total2
            (fun rate_g1 rate_g2 ->
              (Base.dot rate_g1 rate_g2 n_rates))
            site_g1 site_g2)))
      g1 g2)

(* pairwise_product_w_stationary:
 * take the triplewise product of glvs g1, and g2 with the stationary dist then
   * store in dest. *)
let pairwise_product_w_stationary statd dest g1 g2 = 
  ArrayFuns.iter3
    (fun site_dest site_g1 site_g2 ->
      ArrayFuns.iter3
        (fun rate_dest rate_g1 rate_g2 ->
          Base.triplewise_prod rate_dest statd rate_g1 rate_g2)
        site_dest site_g1 site_g2)
    dest g1 g2



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

