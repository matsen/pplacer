(* We keep track of the multiplicities so that we can use them in bootstrapping.
 * Note that one option would be to make the Pres have a float multiplicity,
 * which would mean that we could decide on a transform once and then have that
 * be fixed for the life of the Pre. That would be convenient, but would make
 * bootstrapping, etc, impossible.
*)

open MapsSets

type weighting_choice = Weighted | Unweighted


(* we just return the top one if unweighted *)
let place_list_of_pquery weighting criterion pquery =
  match weighting with
  | Weighted -> Pquery.place_list pquery
  | Unweighted -> [ Pquery.best_place criterion pquery ]


(* Pre as in pre-mass-map *)
module Pre = struct

  type mass_unit =
    {
      loc : int;
      distal_bl : float;
      mass : float;
    }

  let scale_mu scalar mu = {mu with mass = scalar *. mu.mass}

  type multimul = {
    (* multiplicity *)
    multi : int;
    (* mul is Mass Unit List *)
    (* list across mass for a given placement. *)
    mul : mass_unit list;
    }

  let mul_total_mass = List.fold_left (fun x mu -> x +. mu.mass) 0.
  let multimul_total_mass transform mumu =
    (transform mumu.multi) *. (mul_total_mass mumu.mul)
  let scale_multimul scalar mumu =
    {mumu with mul = List.map (scale_mu scalar) mumu.mul}


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
          (Base.normalized_prob (List.map criterion pc));
    }

  (* assume that the list of pqueries in have unit mass. split that mass up to
   * each of the pqueries, breaking it up by weighted placements if desired.
   *)
  let of_pquery_list weighting criterion pql =
    let mass_per_read = 1. /. (float_of_int (Pquery.total_multiplicity pql)) in
    List.map
      (multimul_of_pquery weighting criterion mass_per_read)
      pql

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

  let total_mass transform =
    let f = multimul_total_mass transform in
    List.fold_left (fun x mm -> x +. f mm) 0.

  let normalize_mass transform pre =
    let scalar = 1. /. (total_mass transform pre) in
    List.map (scale_multimul scalar) pre

end


(* Indiv meanins that each unit of mass is considered individually on the tree.
 * Indiv makes the weighting for a given edge as a list of (distal_bl, mass)
 * for each placement.
 *)
module Indiv = struct

         (* distal_bl * mass *)
  type t = (float     * float) IntMap.t

  (* factor is a multiplicative factor to multiply the mass by.
   * transform is an int -> float function which given a multiplicity spits out
   * a float weight as a multiple for the mass. *)
  let of_pre transform ?factor pmm =
    let factorf = match factor with | None -> 1. | Some x -> x in
    List.fold_left
      (fun m' multimul ->
        let scalar = factorf *. (transform multimul.Pre.multi) in
        (List.fold_left
          (fun m mu ->
            IntMapFuns.add_listly
              mu.Pre.loc
              (mu.Pre.distal_bl, scalar *. mu.Pre.mass)
              m)
          m'
          multimul.Pre.mul))
      IntMap.empty
      pmm

  let of_placerun transform weighting criterion pr =
    of_pre transform (Pre.of_placerun weighting criterion pr)

(* sort the placements along a given edge according to their location on
 * the edge in an increasing manner. *)
  let sort m =
    IntMap.map
      (List.sort (fun (a1,_) (a2,_) -> compare a1 a2))
      m

let total_mass m =
  IntMap.fold
    (fun _ mass_l accu ->
      List.fold_right (fun (_, mass) -> ( +. ) mass) mass_l accu)
    m
    0.

  let ppr =
    IntMapFuns.ppr_gen
      (fun ff l ->
        List.iter
          (fun (distal, mass) ->
            Format.fprintf ff "@[{d = %g; m = %g}@]" distal mass)
          l)

end


(* By_edge just considers the weight per edge *)
module By_edge = struct

  type t = float IntMap.t

  let of_indiv =
    IntMap.map (List.fold_left (fun tot (_,weight) -> tot +. weight) 0.)

  (* SPEED
   *
   * a faster version would be like
   *
   let h = Hashtbl.create ((IntMapFuns.nkeys ti_imap)/3) in
    let addto ti x = Hashtbl.replace h ti (x+.(hashtbl_find_zero h ti)) in
    List.iter
    (fun pq ->
      List.iter
      (fun p -> addto (tax_id_of_place p) (criterion p))
        (Pquery.place_list pq))
    (Placerun.get_pqueries pr);
      Mass_map.By_edge.normalize_mass (IntMap.map (hashtbl_find_zero h) ti_imap)
   * *)

  let of_pre transform ?factor pre =
    of_indiv (Indiv.of_pre transform ?factor pre)

  let of_placerun transform weighting criterion pr =
    of_indiv (Indiv.of_placerun transform weighting criterion pr)

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

  let of_placerun_with_zeroes transform weighting criterion pr =
    fill_out_zeroes
      (of_placerun transform weighting criterion pr)
      (Placerun.get_ref_tree pr)

  let total_mass m = IntMap.fold (fun _ v -> ( +. ) v) m 0.

  let normalize_mass m =
    let tot = total_mass m in
    IntMap.map (fun x -> x /. tot) m

end

(* multiplicity transforms for of_pre *)
let no_transform = float_of_int
let unit_transform _ = 1.
let asinh_transform x = Gsl_math.asinh (float_of_int x)

let transform_map =
  List.fold_right
    (fun (k,v) -> StringMap.add k v)
    [
      "", no_transform;
      "unit", unit_transform;
      "asinh", asinh_transform;
    ]
    StringMap.empty

let transform_of_str s =
  try StringMap.find s transform_map with
  | Not_found -> failwith ("Transform "^s^" not known.")
