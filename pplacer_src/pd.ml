open Ppatteries

(* returns if at least two of the descendants have things in them *)
let total_floatol =
  List.fold_left (fun x -> function | Some y -> x+.y | None -> x) 0.

(* compute the PD of an induced tree. we recursively go through the tree,
 * returning Some len if there is a placement distal to us, with len being the
 * length of the path to the last recorded MRCA. *)
let of_induced t ind =
  (* start recording the total branch length from the most distal placement *)
  let perhaps_start_path id =
    match IntMap.opt_find id ind with
    | None -> None (* no path *)
    | Some x -> Some ((Gtree.get_bl t id) -. x)
  in
  (* when we hit an MRCA, we add on the branch lengths here *)
  let total = ref 0. in
  let add_to_tot x = total := x +. !total in
  match
    Gtree.recur
      (fun id below ->
        match List.filter ((<>) None) below with
        | [] -> perhaps_start_path id
        | [Some x] -> Some ((Gtree.get_bl t id) +. x) (* continue path *)
        | _ as l ->
            add_to_tot (total_floatol l); (* record lengths from distal paths *)
            Some (Gtree.get_bl t id)) (* start recording from mrca of those paths *)
      perhaps_start_path
      t
  with
  | Some x -> x +. !total (* add on the last bit of path *)
  | None -> failwith "empty induced tree"

(* later have a lazy data cache with the induceds? *)
let of_pr criterion pr =
  of_induced (Placerun.get_ref_tree pr)
                (Induced.of_placerun criterion pr)

let normalized_of_pr criterion pr =
  (of_pr criterion pr) /. (Gtree.tree_length (Placerun.get_ref_tree pr))
