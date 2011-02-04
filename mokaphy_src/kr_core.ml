(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
*)

open Fam_batteries
open MapsSets

type result = 
  {
    distance : float;
    p_value : float option;
  }

let get_distance r = r.distance
let get_p_value r = match r.p_value with
  | Some p -> p
  | None -> failwith "no p-value!"

(* MULTI: this is going to be trouble. *)
let make_shuffled_pres n_shuffles pre1 pre2 = 
  let pre_arr = Array.of_list (pre1 @ pre2)
  and n1 = List.length pre1
  and n2 = List.length pre2
  in
  let pquery_sub start len = 
    Mass_map.Pre.normalize_mass  
      (Array.to_list (Array.sub pre_arr start len)) 
  in
  ListFuns.init 
    n_shuffles
    (fun _ ->
      Mokaphy_base.shuffle pre_arr;
      (pquery_sub 0 n1, pquery_sub n1 n2))

let pair_core p n_samples t pre1 pre2 =
  let calc_dist = Kr_distance.dist_of_pres p t in
  let original_dist = calc_dist pre1 pre2 in
  {
    distance = original_dist; 
    p_value = 
      if 0 < n_samples then begin
        let shuffled_dists = 
          List.map 
            (fun (spre1,spre2) -> calc_dist spre1 spre2)
            (make_shuffled_pres n_samples pre1 pre2)
        in
        Some
          (Mokaphy_base.list_onesided_pvalue 
            shuffled_dists 
            original_dist)
      end
      else None;
  }

let wrapped_pair_core context p n_samples t pre1 pre2 =
  try pair_core p n_samples t pre1 pre2 with
  | Kr_distance.Invalid_place_loc a -> 
      invalid_arg
        (Printf.sprintf 
          "%g is not a valid placement location when %s" a context)
  | Kr_distance.Total_kr_not_zero tkr ->
      failwith ("total kr_vect not zero for "^context^": "^(string_of_float tkr))


(* core
 * run pair_core for each unique pair 
 *)
let core ch prefs prl = 
  if List.length prl <= 1 then 
    invalid_arg "can't do KR with fewer than two place files";
  let n_samples = Mokaphy_prefs.KR.n_samples prefs 
  and is_weighted = Mokaphy_prefs.KR.weighted prefs
  and use_pp = Mokaphy_prefs.KR.use_pp prefs
  and pra = Array.of_list prl 
  and context pr1 pr2 = 
    Printf.sprintf "comparing %s with %s" 
      (Placerun.get_name pr1) (Placerun.get_name pr2)
  and p = Mokaphy_prefs.KR.p_exp prefs
  and tax_refpkgo = match Mokaphy_prefs.KR.refpkg_path prefs with
      | "" -> None
      | path -> 
        let rp = Refpkg.of_path path in
        if Refpkg.tax_equipped rp then Some rp
        else None
  in
  (* in the next section, pre_f is a function which takes a pr and makes a pre,
   * and t is a gtree *)
  let uptri_of_t_pre_f (t, pre_f) = 
    let prea = Array.map pre_f pra in
    Uptri.init
      (Array.length prea)
      (fun i j ->
        wrapped_pair_core
          (context pra.(i) pra.(j)) p n_samples t prea.(i) prea.(j))
  (* here we make one of these pairs from a function which tells us how to
   * assign a branch length to a tax rank *)
  and t_pre_f_of_bl_of_rank rp bl_of_rank = 
    let (taxt, ti_imap) = Tax_gtree.of_refpkg_gen bl_of_rank rp in
    (Decor_gtree.to_newick_gtree taxt, 
    Cmds_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap)
  in
  (* here we make a list of uptris, which are to get printed *)
  let uptris =
    List.map 
      uptri_of_t_pre_f
      ([Cmds_common.list_get_same_tree prl, 
      Cmds_common.pre_of_pr ~is_weighted ~use_pp] @
      (match tax_refpkgo with
      | None -> []
      | Some rp -> 
          List.map (t_pre_f_of_bl_of_rank rp)
                   [Tax_gtree.unit_bl; Tax_gtree.inverse]))
  (* here are a list of function names to go with those uptris *)
  and fun_names = 
    List.map 
      (fun s -> Printf.sprintf "%s%g" s p)
      (["Z_"] @ 
      (match tax_refpkgo with 
      | Some _ -> ["unit_tax_Z_"; "inv_tax_Z_"] 
      | None -> []))
  (* the names of the placeruns *)
  and names = Array.map Placerun.get_name pra
  and print_pvalues = n_samples > 0
  and neighborly f l = List.flatten (List.map f l)
  in
  Cmds_common.write_uptril 
    (Mokaphy_prefs.KR.list_output prefs)
    names
    (if print_pvalues then neighborly (fun s -> [s;s^"_p_value"]) fun_names 
    else fun_names)
    (if print_pvalues then 
      neighborly (fun u -> [Uptri.map get_distance u; Uptri.map get_p_value u]) uptris 
    else (List.map (Uptri.map get_distance) uptris))
    ch
