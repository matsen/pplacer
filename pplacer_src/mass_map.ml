(* We keep track of the multiplicities so that we can use them in bootstrapping.
 * Note that one option would be to make the Pres have a float multiplicity,
 * which would mean that we could decide on a transform once and then have that
 * be fixed for the life of the Pre. That would be convenient, but would make
 * bootstrapping, etc, impossible.
 *
 * Bootstraping, etc, is also the reason why we have mass_units and multimuls
 * not squashed into a single data type.
*)

open Ppatteries

type weighting_choice = Weighted | Unweighted


(* we just return the top one if unweighted *)
let place_list_of_pquery weighting criterion pquery =
  match weighting with
  | Weighted -> Pquery.place_list pquery
  | Unweighted -> [ Pquery.best_place criterion pquery ]

(* the L_1 norm of a float list *)
let normalized_prob fl =
  let sum = List.fold_left ( +. ) 0. fl in
  List.map (fun x -> x /. sum) fl

(* Pre as in pre-mass-map *)
module Pre = struct

  type mass_unit =
    {
      loc: int;
      distal_bl: float;
      mass: float;
    }

  let scale_mu scalar mu = {mu with mass = scalar *. mu.mass}

  type multimul = {
    (* multiplicity *)
    multi: float;
    (* mul is Mass Unit List *)
    (* list across mass for a given placement. *)
    mul: mass_unit list;
    }

  let mul_total_mass = List.fold_left (fun x mu -> x +. mu.mass) 0.
  let multimul_total_mass mumu =
    mumu.multi *. (mul_total_mass mumu.mul)
  let scale_multimul scalar mumu =
    {mumu with mul = List.map (scale_mu scalar) mumu.mul}
  let unit_mass_scale mumu =
    scale_multimul (1. /. (multimul_total_mass mumu)) mumu


  (* list across pqueries *)
  type t = multimul list

  (* will raise Pquery.Unplaced_pquery if finds unplaced pqueries.  *)
  let multimul_of_pquery weighting criterion mass_per_read pq =
    let pc = place_list_of_pquery weighting criterion pq in
    {
      multi = Pquery.multiplicity pq;
      mul =
        List.map2
          (fun place weight ->
            {
              loc = Placement.location place;
              distal_bl = Placement.distal_bl place;
              mass = mass_per_read *. weight
            })
          pc
          (normalized_prob (List.map criterion pc));
    }

  let of_pquery_list weighting criterion pql =
    let mass_per_read = 1. /. (Pquery.total_multiplicity pql) in
    List.map
      (multimul_of_pquery weighting criterion mass_per_read)
      pql

  (* A unit of mass spread across the tree according to pr. *)
  let of_placerun weighting criterion pr =
    try
      of_pquery_list
        weighting
        criterion
        (Placerun.get_pqueries pr)
    with
    | Pquery.Unplaced_pquery s ->
      invalid_arg ((String.concat " " s)^" unplaced in "^
                     (Placerun.get_name pr))

  let total_mass =
    List.fold_left (fun x mm -> x +. multimul_total_mass mm) 0.

  let scale_mass scalar pre =
    List.map (scale_multimul scalar) pre

  let normalize_mass pre =
    scale_mass (1. /. (total_mass pre)) pre

  let unitize_mass pre =
    List.map unit_mass_scale pre

  (* Pretty printing. *)
  let ppr_mass_unit ff mu =
    Format.fprintf ff "@[{loc = %d; distal_bl = %f; mass = %f}@]"
      mu.loc mu.distal_bl mu.mass

  let ppr_mul ff mul = Ppr.ppr_list ppr_mass_unit ff mul

  let ppr_multimul ff mmul =
    Format.fprintf ff "@[{multi = %g; mul = %a}@]" mmul.multi ppr_mul mmul.mul

  let ppr ff pre = Ppr.ppr_list ppr_multimul ff pre

end


(* Indiv meanins that each unit of mass is considered individually on the tree.
 * Indiv makes the weighting for a given edge as a list of (distal_bl, mass)
 * for each placement.
 *)
module Indiv = struct
  type v = {distal_bl: float; mass: float}
  type t = v list IntMap.t

  let of_pair (bl, mass) = {distal_bl = bl; mass = mass}
  let to_pair {distal_bl = bl; mass = mass} = bl, mass

  (* factor is a multiplicative factor to multiply the mass by. *)
  let of_pre ?factor pmm =
    let factorf = match factor with | None -> 1. | Some x -> x in
    List.fold_left
      (fun m' multimul ->
        let scalar = factorf *. multimul.Pre.multi in
        (List.fold_left
          (fun m mu ->
            IntMap.add_listly
              mu.Pre.loc
              {distal_bl = mu.Pre.distal_bl; mass = scalar *. mu.Pre.mass}
              m)
          m'
          multimul.Pre.mul))
      IntMap.empty
      pmm

  let of_placerun weighting criterion pr =
    of_pre (Pre.of_placerun weighting criterion pr)

(* sort the placements along a given edge according to their location on
 * the edge in an increasing manner. *)
  let sort m =
    IntMap.map
      (List.sort ~cmp:(fun {distal_bl = a1} {distal_bl = a2} -> compare a1 a2))
      m

  let total_mass m =
    IntMap.fold
      (fun _ mass_l accu ->
        List.fold_right (fun {mass = mass} -> ( +. ) mass) mass_l accu)
      m
      0.

  let scale_mass scalar =
    IntMap.map
      (List.map (fun v -> {v with mass = v.mass *. scalar}))

  let work_moving_to vl pos =
    List.fold_left
      (fun tot {distal_bl; mass} ->
        abs_float (distal_bl -. pos) *. mass +. tot)
      0.
      vl

  let v_mass = List.fold_left (fun tot {mass} -> tot +. mass) 0.

  let ppr =
    IntMap.ppr_gen
      (fun ff l ->
        List.iter
          (fun {distal_bl = distal; mass = mass} ->
            Format.fprintf ff "@[{d = %g; m = %g}@]" distal mass)
          l)

end


(* By_edge just considers the weight per edge *)
module By_edge = struct

  type t = float IntMap.t

  let of_indiv =
    IntMap.map
      (List.fold_left (fun tot {Indiv.mass = weight} -> tot +. weight) 0.)

  (* SPEED
   *
   * a faster version would be like
   *
   let h = Hashtbl.create ((IntMap.nkeys ti_imap)/3) in
    let addto ti x = Hashtbl.replace h ti (x+.(hashtbl_find_zero h ti)) in
    List.iter
    (fun pq ->
      List.iter
      (fun p -> addto (tax_id_of_place p) (criterion p))
        (Pquery.place_list pq))
    (Placerun.get_pqueries pr);
      Mass_map.By_edge.normalize_mass (IntMap.map (hashtbl_find_zero h) ti_imap)
   * *)

  let of_pre ?factor pre =
    of_indiv (Indiv.of_pre ?factor pre)

  let of_placerun weighting criterion pr =
    of_indiv (Indiv.of_placerun weighting criterion pr)

  (* we add zeroes in where things are empty *)
  let fill_out_zeroes mass_map ref_tree =
    let rec aux accu = function
      | loc::rest ->
        aux
          (if IntMap.mem loc accu then accu
          else IntMap.add loc 0. accu)
          rest
      | [] -> accu
    in
    aux
      mass_map
      (Stree.node_ids (Gtree.get_stree ref_tree))

  let of_placerun_with_zeroes weighting criterion pr =
    fill_out_zeroes
      (of_placerun weighting criterion pr)
      (Placerun.get_ref_tree pr)

  let total_mass m = IntMap.fold (fun _ v -> ( +. ) v) m 0.

  let normalize_mass m =
    let tot = total_mass m in
    IntMap.map (fun x -> x /. tot) m

end

(* multiplicity transforms for of_pre *)
let no_transform = identity
let unit_transform _ = 1.
let log_transform x = log x
let asinh_transform x = Gsl_math.asinh x

let transform_map = StringMap.of_pairlist
  [
    "", no_transform;
    "no_trans", no_transform;
    "unit", unit_transform;
    "log", log_transform;
    "asinh", asinh_transform;
  ]

let transform_of_str s =
  try StringMap.find s transform_map with
  | Not_found -> failwith ("Transform "^s^" not known.")
