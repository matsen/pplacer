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

exception Invalid_place_loc of float

type kr_result = { distance : float }

let tol = 1e-50 (* "zero" *)

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


(* add v2 to v1 (which is modified in place) *)
let v_addto v1 v2 = 
  for i=0 to (Array.length v1)-1 do
    v1.(i) <- v1.(i) +. v2.(i)
  done

(* take the sum of a v list. no side effects. *)
let v_list_sum = function
  | hd::tl ->
    let v = Array.copy hd in
    List.iter (v_addto v) tl;
    v
  | [] -> assert(false)


let pair_core verbose cmp ch ref_tree (npcl_name1,npcl1) (npcl_name2,npcl2) = 
  let get_map name npcl = 
    let (m,u) = Placement.sorted_npcl_map_by_best_loc_of_npc_list cmp npcl in
    if u <> [] then invalid_arg (name^" has unplaced sequences!")
    else m
  in
  let m1 = get_map npcl_name1 npcl1
  and m2 = get_map npcl_name2 npcl2
  in
  let int_inv x = 1. /. (float_of_int x) in
  (* these two may take arguments in the future *)
  let kr_v1 = [|int_inv (List.length npcl1); 0.|]
  and kr_v2 = [|0.; int_inv (List.length npcl2)|]
  in
  (* collect the information we care about for KR: for each location, get the
   * sequences whose best placements are there, along with their attachment
   * locations (distal_bl) and the kr_v, which is the vector to use when adding
   * up placements *)
  let collect_kr_info kr_v npcl_map =
    IntMap.map
      (List.map
        (function (_,pc) -> (* forget the name *)
          match pc with
          | best::_ -> Placement.distal_bl best, kr_v
          | [] -> assert(false)))
      npcl_map
  in
  (* this map has all of the information needed to do the KR calculation *)
  let all_kr_map = 
    IntMap.map
      (fun ll -> 
        (* sort the placements along a given edge according to their location on
         * the edge. that way we can recur along this list. *)
        List.sort
          (fun (a1,_) (a2,_) -> compare a1 a2)
          (List.flatten ll))
      (Base.combine_intmaps_listly
        [collect_kr_info kr_v1 m1; collect_kr_info kr_v2 m2])
  in
  if verbose then begin
    ppr_kr_info Format.std_formatter all_kr_map;
    Format.pp_print_newline Format.std_formatter ()
  end;
  (* kr_diff is how we take the difference of the two prob dists *)
  let kr_diff kr_v = abs_float (kr_v.(0) -. kr_v.(1)) in
  (* total up the info from the previous step. 
   * note that kr_v_sofar will be modified in place. *)
  let total_along_edge prev_subtot start_kr_v bl kr_info_list = 
    let rec aux ~subtotal ~prev_a kr_v_sofar kr_info_list = 
      (* next_total actually adds on the segment length times the kr_diff of the
       * kr vector *)
      let next_subtotal a =
        let seg_len = a -. prev_a in
        (* Printf.printf "%g\t%g\n" prev_a a; *)
        assert(seg_len >= 0.);
        subtotal+.seg_len*.(kr_diff kr_v_sofar)
      in
      match kr_info_list with
      | (a, kr_v)::rest -> 
          (* we pull this out so that we do the next total, then add on the kr_v
           * onto the kr_v_sofar *)
          if a < 0. || a > bl then raise (Invalid_place_loc a);
          let the_next_subtotal = next_subtotal a in
          aux
            ~subtotal:the_next_subtotal
            ~prev_a:a
            (v_addto kr_v_sofar kr_v; kr_v_sofar)
            rest
      | [] -> 
          (* sum things up on final segment to the next node *)
          (next_subtotal bl, kr_v_sofar)
    in
    aux prev_subtot 0. start_kr_v kr_info_list
  in
  (* total across all of the edges of the tree *)
  let starter_kr_v = [|0.; 0.|]
  and get_kr_info id = 
    if IntMap.mem id all_kr_map 
      then IntMap.find id all_kr_map
      else []
  in
  let (grand_total, final_kr_v) = 
    try
      Stree.recur 
        (fun id below_list -> (* the node recurrence *)
          total_along_edge 
            (List.fold_right ( +. ) (List.map fst below_list) 0.) (* prev subtot *)
            (v_list_sum (List.map snd below_list)) (* total of below kr_infos *)
            (Stree.get_bl ref_tree id)
            (get_kr_info id))
        (fun id ->
          total_along_edge 
            0. 
            (Array.copy starter_kr_v) 
            (Stree.get_bl ref_tree id) (* code dup, but not worth factoring... couldn't use recur then *)
            (get_kr_info id))
        ref_tree
    with
    | Invalid_place_loc a -> 
        invalid_arg
          (Printf.sprintf 
            "%g is not a valid placement location when comparing %s with %s"
            a npcl_name1 npcl_name2)
  in
  if abs_float (kr_diff final_kr_v) > tol then 
    failwith ("total kr_vect not zero for "^npcl_name1^" versus "^npcl_name2);
  Printf.fprintf ch "%s\t%s\t%g\n" npcl_name1 npcl_name2 grand_total;
  grand_total


(* core
 * run pair_core for each unique pair 
 *)
let core verbose cmp ch ref_tree nplacecoll_arr = 
  let u = 
    Uptri.init
      (Array.length nplacecoll_arr)
      (fun i j ->
        pair_core verbose cmp ch 
         ref_tree nplacecoll_arr.(i) nplacecoll_arr.(j))
  in
  Uptri.print_float_uptri u
