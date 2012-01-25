open Subcommand
open Guppy_cmdobjs
open Ppatteries

let epsilon = 1e-10

let merge a x = a := x +. !a
(* float ref list -> float ref: sum a list of float refs, returning a new float
 * ref. *)
let lmerge = List.fold_left ((!) |- (+.) |> flip) 0. |- ref

module I = Mass_map.Indiv

(* convenience function to total the mass along the tree. the callback function
 * is passed the branch length and the current mass total and returns another
 * term to sum. the result of total_along_mass is the sum of all the terms. *)
let total_along_mass gt mass cb =
  let partial_total id = Kr_distance.total_along_edge
    cb
    (Gtree.get_bl gt id)
    (IntMap.get id [] mass |> List.map I.to_pair |> List.sort compare)
    merge
  in
  Kr_distance.total_over_tree
    partial_total
    (const ())
    lmerge
    (fun () -> ref 0.)
    (Gtree.get_stree gt)

(* When we're passing along the induced tree, the mass will be on the range
 * (0, total_mass), and the branch length coefficient should be 1. Otherwise,
 * we're either before or after the induced tree and the multiplier should
 * be 0. *)
let bump_function r =
  if r = 0. || approx_equal ~epsilon r 1. then 0. else 1.

let pd_of_placerun criterion pr =
  let gt = Placerun.get_ref_tree pr
  and mass = I.of_placerun
    Mass_map.Point
    criterion
    pr
  in
  total_along_mass
    gt
    mass
    (fun r bl -> bump_function !r *. bl)

let reflect x =
  if approx_equal ~epsilon x 1. then 0.
  else begin
    let y = 1. -. x in
    if y < 0. then failwith "reflect out of range!"
    else y
  end

let wpd_of_placerun criterion exponent pr =
  let mass_map = Mass_map.Indiv.of_placerun Mass_map.Point criterion pr in
  let f =
    if exponent < 0. || exponent > 1. then
      failwith("exponent must be between 0 and 1, inclusive")
    else if exponent = 1. then (fun x -> 2. *. x)
    else if exponent = 0. then bump_function
    else (fun x -> (2. *. x) ** exponent)
  in
  total_along_mass
    (Placerun.get_ref_tree pr)
    mass_map
    (fun r bl -> min (f !r) (f (reflect !r)) *. bl)

let entropy_of_placerun criterion pr =
  let total = total_along_mass
    (Placerun.get_ref_tree pr)
    (Mass_map.Indiv.of_placerun Mass_map.Point criterion pr)
  in
  let phylo r bl = if approx_equal !r 0. then 0. else bl *. !r *. (log !r)
  and quadro r bl = bl *. !r *. (1. -. !r) in
  ~-. (total phylo), total quadro

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val kappa = flag "--kappa"
    (Plain ([], "A comma-separated list of additional exponents to use for calculating wpd."))

  method specl =
    super_mass#specl
    @ super_tabular#specl
    @ [
      string_list_flag kappa;
    ]

  method desc = "calculates various diversity metrics of placefiles"
  method usage = "usage: fpd [options] placefile[s]"

  method private placefile_action prl =
    let exponents = fv kappa
      |> List.map (flip String.nsplit ",")
      |> List.flatten
      |> List.map float_of_string
    and criterion = self#criterion in
    let wpd = wpd_of_placerun criterion
    and pd = pd_of_placerun criterion
    and entropy = entropy_of_placerun criterion in
    prl
      |> List.map
          (fun pr ->
            let pe, qe = entropy pr in
            [pe; qe; pd pr; wpd 1. pr]
            |> List.append (List.map (flip wpd pr) exponents)
            |> List.map (Printf.sprintf "%g")
            |> List.cons (Placerun.get_name pr))
      |> List.cons
          (["placerun"; "phylogenetic"; "quadratic"; "pd"; "wpd"]
           @ List.map (Printf.sprintf "wpd_%g") exponents)
      |> self#write_ll_tab

end
