(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

(*
for each edge, collect placements as a list of 
(location along edge, vector showing which one it is)
then take sums of these, and multiply them by length of segment

keep in mind:
at some point we may want to customize weight of the placements
we may or may not want to have the pendant branch lengths
we want to make a randomization procedure

*)
open Fam_batteries
open MapsSets
open MapsSets
open Mokaphys_prefs

exception Invalid_place_loc of float
exception Unplaced_sequences
exception Total_kr_not_zero of float

type kr_result = { distance : float }

let tol = 1e-10 (* "zero" *)

let ppr_kr_info ff info_map = 
  IntMapFuns.ppr_gen
    (Ppr.ppr_list
      (fun ff (bl, v) -> 
        Format.fprintf ff 
          "(%g, %a)" 
          bl
          Ppr.ppr_float_array v))
    ff
    info_map

let ppr_pair_float_uptri ff fu = 
  Uptri.ppr_uptri 
    (fun ff (x,y) -> Format.fprintf ff "(%g, %g)" x y)
    ff 
    fu

let write_named_float_uptri ch names u =
  String_matrix.write_named_padded ch names
    (MatrixFuns.map 
      (Printf.sprintf "%g")
      (Uptri.to_matrix (fun _ -> 0.) u))


(* add v2 to v1 (which is modified in place) *)
let v_addto v1 v2 = 
  for i=0 to (Array.length v1)-1 do
    v1.(i) <- v1.(i) +. v2.(i)
  done

(* multiply a vector times a scalar. functional. *)
let v_times_scalar v s =
  Array.map (( *. ) s) v

(* take the vector sum of a float array list. no side effects. *)
let v_list_sum = function
  | hd::tl ->
    let v = Array.copy hd in
    List.iter (v_addto v) tl;
    v
  | [] -> assert(false)

(* the KR exponent is 1/p unless p is less than 1, in which case it's 1 *)
let outer_exponent p = 
  if p < 1. then 1.
  else 1. /. p

(* get from map, but return an empty list if not in map *)
let get_from_list_intmap m id = 
  if IntMap.mem id m then IntMap.find id m
  else []

  (*
let get_npc_map criterion npcl = 
  let (u,m) = Placement.sorted_npcl_map_by_best_loc_of_npc_list criterion npcl in
  if u <> [] then raise Unplaced_sequences
  else m

(* collect the information we care about for KR: for each location, get the
 * sequences whose best placements are there, along with their attachment
 * locations (distal_bl) and the kr_v, which is the vector to use when adding
 * up placements *)
let collect_kr_info criterion kr_v npcl_map =
  IntMap.map
    (List.map
      (function (_,pc) -> (* forget the name *)
        (* non placement-weighted version
      *)
        match pc with
        | best::_ -> [Placement.distal_bl best, kr_v]
        | [] -> assert(false)
        (*
        List.map2
          (fun place weight ->
            Placement.distal_bl place, 
            v_times_scalar kr_v weight)
          pc
          (Base.normalized_prob (List.map criterion pc))
      *)
      ))
    npcl_map
  *)

  (*
  non placement-weighted version
let collect_kr_info criterion kr_v npcl =
  List.map
    (function (_,pc) -> (* forget the name *)
        match Placement.sort_placecoll criterion pc with
        | best::_ -> 
            (Placement.location best,
              (Placement.distal_bl best, kr_v))
        | [] -> assert(false)
        )
    npcl

    *)

let collect_kr_info criterion kr_v npcl =
  List.flatten
    (List.map
      (function (_,pc) -> (* forget the name *)
        List.map2
          (fun place weight ->
            (Placement.location place,
            (Placement.distal_bl place, 
            v_times_scalar kr_v weight)))
          pc
          (Base.normalized_prob (List.map criterion pc)))
       npcl)
 
 

  (* exp_kr_diff is the difference of the two prob dists to the pth pow *)
let exp_kr_diff p kr_v = (abs_float (kr_v.(0) -. kr_v.(1))) ** p

(* total up the info from the previous step. 
 * note that data_sofar will be modified in place. *)
let total_along_edge data_to_r bl data_info_list update_data prev_subtot start_data = 
  let rec aux ~subtotal ~prev_a data_sofar data_info_list = 
    (* next_total actually adds on the segment length times data_to_r of
     * the kr vector *)
    let next_subtotal a =
      let seg_len = a -. prev_a in
      (* Printf.printf "%g\t%g\n" prev_a a; *)
      assert(seg_len >= 0.);
      subtotal+.seg_len*.(data_to_r data_sofar)
    in
    match data_info_list with
    (* a is the location of the location of the data along the edge *)
    | (a, data)::rest -> 
        (* we pull this out so that we do the next total, then add on the data
         * onto the data_sofar *)
        if a < 0. || a > bl then raise (Invalid_place_loc a);
        let the_next_subtotal = next_subtotal a in
        aux
          ~subtotal:the_next_subtotal
          ~prev_a:a
          (update_data data_sofar data; data_sofar)
          rest
    | [] -> 
        (* sum things up on final segment to the next node *)
        (next_subtotal bl, data_sofar)
  in
  aux prev_subtot 0. start_data data_info_list

(* total some data over the tree, which can be combined from subtrees using
 * data_list_sum, and can be totaled across edges using curried_edge_total, and
 * starts at leaves with starter_data_factory.
 * the reason why we use starter_data_factory rather than doing a fully
 * functional approach is that then the number of allocations is linear in only
 * the size of the tree, rather than depending on the number of placements .
 * *)
let total_over_tree curried_edge_total
                    check_final_data
                    data_list_sum
                    starter_data_factory 
                    ref_tree =
  let (grand_total, final_data) = 
    Stree.recur 
      (fun id below_list -> (* the node recurrence *)
        curried_edge_total 
          id
          (List.fold_right ( +. ) (List.map fst below_list) 0.) (* prev subtot *)
          (data_list_sum (List.map snd below_list))) (* total of below kr_infos *)
      (fun id ->
        curried_edge_total 
          id
          0. 
          (starter_data_factory ()))
      ref_tree
  in
  check_final_data final_data;
  grand_total /. (Stree.tree_length ref_tree)

  (* get the KR distance between two named placement collection lists *)
let pair_distance criterion ref_tree p npcl1 npcl2 = 
  (*
  let m1 = get_npc_map criterion npcl1
  and m2 = get_npc_map criterion npcl2
  in
  *)
  let int_inv x = 1. /. (float_of_int x) in
  (* these two may take arguments in the future *)
  let kr_v1 = [|int_inv (List.length npcl1); 0.|]
  and kr_v2 = [|0.; int_inv (List.length npcl2)|]
  in
  (* this map has all of the information needed to do the KR calculation *)
  let all_kr_map = 
    IntMap.map
        (* sort the placements along a given edge according to their location on
         * the edge. that way we can recur along this list. *)
      (List.sort (fun (a1,_) (a2,_) -> compare a1 a2))
      (Base.intMap_of_pairlist_listly
        ((collect_kr_info criterion kr_v1 npcl1) @
         (collect_kr_info criterion kr_v2 npcl2)))
  in
  (* ppr_kr_info Format.std_formatter all_kr_map; Format.pp_print_newline Format.std_formatter ()
  *)
  (* total across all of the edges of the tree *)
  let starter_kr_v = [|0.; 0.|]
   in
  let kr_edge_total id = 
    total_along_edge 
      (exp_kr_diff p) 
      (Stree.get_bl ref_tree id) 
      (get_from_list_intmap all_kr_map id)
      v_addto
  (* make sure that the kr_v totals to zero *)
  and check_final_kr final_kr_v = 
    let final_kr_diff = final_kr_v.(0) -. final_kr_v.(1) in
    if abs_float final_kr_diff > tol then 
      raise (Total_kr_not_zero final_kr_diff)
  in
  (total_over_tree 
    kr_edge_total
    check_final_kr
    v_list_sum
    (fun () -> Array.copy starter_kr_v)
    ref_tree)
  ** (outer_exponent p)

let pair_core prefs criterion ref_tree (npcl_name1,npcl1) (npcl_name2,npcl2) = 
  let context = 
    Printf.sprintf "comparing %s with %s" npcl_name1 npcl_name2 
  and ref_tree_len = Stree.tree_length ref_tree in
  try
    (* Printf.fprintf ch "%s\t%s\t%g\n" npcl_name1 npcl_name2 grand_total; *)
    let placement_arr = Array.of_list (npcl1 @ npcl2)
    and list_of_sub_array a start len = 
      Array.to_list (Array.sub a start len)
    in
    let to_shuffle = Array.copy placement_arr in
    let calc_dist places p = 
        pair_distance criterion ref_tree p
            (list_of_sub_array places 0 (List.length npcl1))
            (list_of_sub_array places 
              (List.length npcl1) (List.length npcl2)) 
    in
    (* calculate unshuffled distance *)
    let chosen_p = p_exp prefs in
    let sample_dist = calc_dist placement_arr chosen_p in
    let shuff_dists =
      Array.init 
        (n_shuffles prefs)
        (fun _ ->
          Base.shuffle to_shuffle;
          calc_dist to_shuffle chosen_p)
    in
    Array.sort compare shuff_dists;
    (* maybe write out histogram files *)
    if histo prefs then begin
      let histo_prefix = "histo."^npcl_name1^".VS."^npcl_name2 in
      (* the data *)
      let histo_dat_name = histo_prefix^".dat" in
      let histo_dat_ch = open_out histo_dat_name in
      Array.iter 
        (fun x -> Printf.fprintf histo_dat_ch "%g\n" x) 
        shuff_dists;
      close_out histo_dat_ch;
      (* the r file *)
      let histo_r_ch = open_out (histo_prefix^".r") in
      Printf.fprintf histo_r_ch "pdf(\"%s\")\n" (histo_prefix^".pdf");
      Printf.fprintf histo_r_ch "data <- read.table(\"%s\")\n" histo_dat_name;
      Printf.fprintf histo_r_ch 
                     "hist(data[,1], main=\"d(%s,%s) = %f\", xlab=\"KR Z_%g distance\")\n" 
                     npcl_name1 npcl_name2 sample_dist chosen_p;
      Printf.fprintf histo_r_ch "abline(v=%g, col=\"red\")\n" sample_dist;
      Printf.fprintf histo_r_ch "dev.off()\n";
      close_out histo_r_ch;
    end;
    if p_plot prefs then begin
      let p_plot_prefix = "p_plot."^npcl_name1^".VS."^npcl_name2 in
      (* the data *)
      let p_plot_dat_name = p_plot_prefix^".dat" in
      let p_plot_dat_ch = open_out p_plot_dat_name in
      let n_samples = 101
      and min_sample = 1e-3
      and max_sample = 1e+3 in
      let p_arr = 
        Base.logarithmically_evenly_spaced 
          n_samples min_sample max_sample in
      Array.iter 
        (fun p -> 
          Printf.fprintf 
            p_plot_dat_ch 
            "%g\t%g\n" 
            p 
            ((calc_dist placement_arr p) /. 
             (ref_tree_len ** (outer_exponent p))))
        p_arr;
      close_out p_plot_dat_ch;
      (* the r file *)
      let p_plot_r_ch = open_out (p_plot_prefix^".r") in
      Printf.fprintf p_plot_r_ch "pdf(\"%s\")\n" (p_plot_prefix^".pdf");
      Printf.fprintf p_plot_r_ch "data <- read.table(\"%s\")\n" p_plot_dat_name;
      Printf.fprintf p_plot_r_ch 
                     "plot(data, main=\"%s versus %s\", xlab=\"p\", ylab=\"normalized KR p-distance\", log=\"x\")\n" 
                     npcl_name1 npcl_name2;
      (* Printf.fprintf p_plot_r_ch "abline(v=1, col=\"red\")\n"; *)
      Printf.fprintf p_plot_r_ch "dev.off()\n";
      close_out p_plot_r_ch;
    end;

    let p_value = Base.arr_onesided_pvalue shuff_dists sample_dist in
    (* Printf.fprintf ch "%s\t%s\t%g\t%g\n" npcl_name1 npcl_name2 sample_dist
     * p_value; *)
    (sample_dist, p_value)
  with
  | Invalid_place_loc a -> 
      invalid_arg
        (Printf.sprintf 
          "%g is not a valid placement location when %s" a context)
  | Unplaced_sequences ->
      invalid_arg ("Unplaced sequences when "^context)
  | Total_kr_not_zero tkr ->
      failwith ("total kr_vect not zero for "^context^": "^(string_of_float tkr))


(* core
 * run pair_core for each unique pair 
 *)
let core prefs criterion ch ref_tree nplacecoll_arr = 
  Printf.printf "calculating Z_%g distance...\n" (p_exp prefs);
  let u = 
    Uptri.init
      (Array.length nplacecoll_arr)
      (fun i j ->
        pair_core prefs criterion
         ref_tree nplacecoll_arr.(i) nplacecoll_arr.(j))
  in
  let names = Array.map fst nplacecoll_arr in
  Printf.fprintf ch "distances\n"; 
  write_named_float_uptri ch names (Uptri.map fst u);
  Printf.fprintf ch "\np-values\n"; 
  write_named_float_uptri ch names (Uptri.map snd u);
