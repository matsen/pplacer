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

(* makes an array of shuffled placeruns (identity of being in first or second
 * one shuffled randomly, but number in each the same) *)
let make_shuffled_prs n_shuffles pr1 pr2 = 
  let pq1 = Placerun.get_pqueries pr1
  and pq2 = Placerun.get_pqueries pr2
  in
  let pquery_arr = Array.of_list (pq1 @ pq2)
  and n1 = List.length pq1
  and n2 = List.length pq2
  in
  let pquery_sub start len = 
    Array.to_list (Array.sub pquery_arr start len)
  in
  let make_pr pr num pqueries = 
    Placerun.make
      (Placerun.get_ref_tree pr)
      (Placerun.get_prefs pr)
      ((Placerun.get_name pr)^"_shuffle_"^(string_of_int num))
      pqueries
  in
  ListFuns.init 
    n_shuffles
    (fun num ->
      Mokaphy_base.shuffle pquery_arr;
      (make_pr pr1 num (pquery_sub 0 n1),
      make_pr pr2 num (pquery_sub n1 n2)))

let weighting_of_prefs prefs =
  if Mokaphy_prefs.KR.weighted prefs then Mass_map.Weighted 
  else Mass_map.Unweighted 

let pair_core prefs criterion pr1 pr2 =
  (*
  let rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
  Gsl_rng.set rng (Nativeint.of_int (Mokaphy_prefs.KR.seed prefs));
  *)
  let p = (Mokaphy_prefs.KR.p_exp prefs) in
  let weighting = weighting_of_prefs prefs in
  if Mokaphy_prefs.KR.ddensity prefs then
    R_plots.write_ddensity pr1 pr2;
  (*
   * disabled matrix
  if Mokaphy_prefs.KR.matrix prefs then begin
    let (distance, p_value) =
      Matrix_sig.dist_and_p weighting criterion rng pr1 pr2 in
    {distance = distance; p_value = Some p_value}
  end
  else 
    if Mokaphy_prefs.KR.normal prefs then begin
      (* normal approx mode *)
      if 0 >= Mokaphy_prefs.KR.n_samples prefs then
        failwith "Please ask for some number of normal samples greater than zero. If you want to disable sampling do not use the --normal option";
      let resampled_dists = 
        Normal_approx.normal_pair_approx rng weighting
        criterion (Mokaphy_prefs.KR.n_samples prefs) p pr1 pr2
      in
      (* here we shadow original_dist with one we know is unweighted *)
      let original_dist = 
        Kr_distance.pair_distance
        weighting
        criterion 
        p 
        pr1 
        pr2
    in
    R_plots.write_density 
      "normal"
      (Placerun.get_name pr1)
      (Placerun.get_name pr2)
      original_dist 
      resampled_dists
      p;
    { distance = original_dist;
      p_value = 
        Some 
          (Mokaphy_base.list_onesided_pvalue 
            resampled_dists 
            original_dist)}
  end
  else 
    *)
    begin
    let calc_dist = 
      Kr_distance.pair_distance
        weighting 
        criterion 
        p in
    let original_dist = calc_dist pr1 pr2 in
    (* shuffle mode *)
    let p_value = 
      if 0 < Mokaphy_prefs.KR.n_samples prefs then begin
        let shuffled_list = 
          make_shuffled_prs (Mokaphy_prefs.KR.n_samples prefs) pr1 pr2 in
        let shuffled_dists = 
          List.map 
            (fun (spr1,spr2) -> calc_dist spr1 spr2)
            shuffled_list
        in
        if Mokaphy_prefs.KR.density prefs then
          R_plots.write_density 
            "shuffle_density"
            (Placerun.get_name pr1)
            (Placerun.get_name pr2)
            original_dist 
            shuffled_dists 
            p;
        if Mokaphy_prefs.KR.p_plot prefs then
          R_plots.write_p_plot weighting criterion pr1 pr2;
        if Mokaphy_prefs.KR.box_plot prefs then
          R_plots.write_boxplot weighting criterion pr1 pr2 shuffled_list;
        (* make a barycenter plot of significance *)
        if Mokaphy_prefs.KR.bary_density prefs then begin
          let bary_dist xpr1 xpr2 = 
            Barycenter.calc_dist
              (weighting_of_prefs prefs)
              criterion 
              xpr1
              xpr2
          in
          let original_bdist = bary_dist pr1 pr2
          and shuffled_bdists = 
            List.map 
              (fun (spr1,spr2) -> calc_dist spr1 spr2)
              shuffled_list
          in
          R_plots.write_density 
            "bary_density"
            (Placerun.get_name pr1)
            (Placerun.get_name pr2)
            original_bdist 
            shuffled_bdists 
            p;
        end;
        (* return the KR p-value *)
        Some
          (Mokaphy_base.list_onesided_pvalue 
            shuffled_dists 
            original_dist)
      end
      else None
    in
    {distance = original_dist; p_value = p_value}
  end


let wrapped_pair_core prefs criterion pr1 pr2 =
  let context = 
    Printf.sprintf "comparing %s with %s" 
      (Placerun.get_name pr1) (Placerun.get_name pr2)
  in
  try
    pair_core prefs criterion pr1 pr2
  with
  | Kr_distance.Invalid_place_loc a -> 
      invalid_arg
        (Printf.sprintf 
          "%g is not a valid placement location when %s" a context)
  | Pquery.Unplaced_pquery s ->
      invalid_arg (s^" unplaced when "^context)
  | Kr_distance.Total_kr_not_zero tkr ->
      failwith ("total kr_vect not zero for "^context^": "^(string_of_float tkr))

(* core
 * run pair_core for each unique pair 
 *)
let core ch prefs criterion pr_arr = 
  if Array.length pr_arr > 1 then begin
    let u = 
      Uptri.init
        (Array.length pr_arr)
        (fun i j ->
          wrapped_pair_core
            prefs
            criterion
            pr_arr.(i) 
            pr_arr.(j))
    in
    (* matrix funniness *)
    let names = Array.map Placerun.get_name pr_arr 
      (* if Mokaphy_prefs.KR.matrix prefs then 2. else  *)
    and p_exp = Mokaphy_prefs.KR.p_exp prefs
    and print_pvalues = (* Mokaphy_prefs.KR.matrix prefs || *)
                        Mokaphy_prefs.KR.n_samples prefs > 0
    in
    if Mokaphy_prefs.KR.list_output prefs then begin
      String_matrix.write_padded ch
        (Array.append
          [|Array.append
            [|"sample_1"; "sample_2"; Printf.sprintf "Z_%g" p_exp;|]
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
      Printf.fprintf ch "Z_%g distances:\n" p_exp;
      Mokaphy_base.write_named_float_uptri ch names (Uptri.map get_distance u);
      if Mokaphy_prefs.KR.matrix prefs || Mokaphy_prefs.KR.n_samples prefs > 0 then begin
        Printf.fprintf ch "Z_%g p-values:\n" p_exp;
        Mokaphy_base.write_named_float_uptri ch names (Uptri.map get_p_value u);
      end
    end;
  end;
  ()

