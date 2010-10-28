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
  in
  let (t, my_pre_of_pr) = 
    match Mokaphy_prefs.KR.refpkg_path prefs with
    | "" ->
        (Cmds_common.list_get_same_tree prl, Cmds_common.pre_of_pr ~is_weighted ~use_pp)
    | path -> begin
        let rp = Refpkg.of_path path in
        let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
        (Decor_gtree.to_newick_gtree taxt, 
        Cmds_common.make_tax_pre ~is_weighted ~use_pp ti_imap)
    end
  and pra = Array.of_list prl in
  let prea = Array.map my_pre_of_pr pra in
  let context pr1 pr2 = 
    Printf.sprintf "comparing %s with %s" 
      (Placerun.get_name pr1) (Placerun.get_name pr2)
  and p = Mokaphy_prefs.KR.p_exp prefs
  in
  let u = 
    Uptri.init
      (Array.length prea)
      (fun i j ->
        wrapped_pair_core
          (context pra.(i) pra.(j)) p n_samples t prea.(i) prea.(j))
  in
  (* matrix funniness *)
  let names = Array.map Placerun.get_name pra
    (* if Mokaphy_prefs.KR.matrix prefs then 2. else  *)
  and print_pvalues = (* Mokaphy_prefs.KR.matrix prefs || *)
                      n_samples > 0
  in
  if Mokaphy_prefs.KR.list_output prefs then begin
    String_matrix.write_padded ch
      (Array.append
        [|Array.append
          [|"sample_1"; "sample_2"; Printf.sprintf "Z_%g" p;|]
          (if print_pvalues then [|"p_values"|] else [||])|]
        (let m = ref [] in
        Uptri.iterij
          (fun i j r -> 
            m := 
              (Array.of_list
                ([names.(i); names.(j); string_of_float r.distance] @
                (if print_pvalues then [string_of_float (get_p_value r)]
                else [])))::!m)
          u;
        Array.of_list (List.rev !m)))
  end
  else begin
    Printf.fprintf ch "Z_%g distances:\n" p;
    Mokaphy_base.write_named_float_uptri ch names (Uptri.map get_distance u);
    if Mokaphy_prefs.KR.matrix prefs || Mokaphy_prefs.KR.n_samples prefs > 0 then begin
      Printf.fprintf ch "Z_%g p-values:\n" p;
      Mokaphy_base.write_named_float_uptri ch names (Uptri.map get_p_value u);
    end
  end;
  ()


