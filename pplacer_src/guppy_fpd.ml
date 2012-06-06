open Subcommand
open Guppy_cmdobjs
open Ppatteries

let epsilon = 1e-10

let merge a x = a := x +. !a
(* float ref list -> float ref: sum a list of float refs, returning a new float
 * ref. *)
let lmerge = List.fold_left ((!) |- (+.) |> flip) 0. |- ref

module I = Mass_map.Indiv
module P = Mass_map.Pre

(* convenience function to total the mass along the tree. the callback function
 * is passed the branch length and the current mass total and returns another
 * term to sum. the result of total_along_mass is the sum of all the terms. *)
let total_along_mass ?(include_pendant = false) criterion pr cb =
  let gt = Placerun.get_ref_tree pr |> Newick_gtree.add_zero_root_bl in
  let pre = P.of_placerun Mass_map.Point criterion pr in
  let mass = I.of_pre pre in
  let partial_total id = Kr_distance.total_along_edge
    ((!) |- cb)
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
  |> (if not include_pendant then identity else fun v ->
    List.fold_left
      (fun accum {P.multi; P.mul} ->
        List.fold_left
          (fun accum {P.pendant_bl; P.mass} ->
            accum +. cb (mass *. multi) pendant_bl)
          accum
          mul)
      v
      pre)

(* When we're passing along the induced tree, the mass will be on the range
 * (0, total_mass), and the branch length coefficient should be 1. Otherwise,
 * we're either before or after the induced tree and the multiplier should
 * be 0. *)
let bump_function r =
  if r =~ 0. || approx_equal ~epsilon r 1. then 0. else 1.

let bump_with_root r =
  if r =~ 0. then 0. else 1.

let pd_of_placerun ?include_pendant ?(bump = bump_function) criterion pr =
  total_along_mass
    ?include_pendant
    criterion
    pr
    (fun r bl -> bump r *. bl)

let reflect x =
  if approx_equal ~epsilon x 1. then 0.
  else begin
    let y = 1. -. x in
    if y < 0. then failwith "reflect out of range!"
    else y
  end

let awpd_of_placerun ?include_pendant criterion exponent pr =
  let f =
    if exponent < 0. || exponent > 1. then
      failwith("exponent must be between 0 and 1, inclusive")
    else if exponent = 1. then (fun x -> 2. *. x)
    else if exponent = 0. then bump_function
    else (fun x -> (2. *. x) ** exponent)
  in
  total_along_mass
    ?include_pendant
    criterion
    pr
    (fun r bl -> min (f r) (f (reflect r)) *. bl)

let entropy_of_placerun ?include_pendant criterion pr =
  let total = total_along_mass ?include_pendant criterion pr in
  let phylo r bl = if approx_equal r 0. then 0. else bl *. r *. (log r)
  and quadro r bl = bl *. r *. (1. -. r) in
  ~-. (total phylo), total quadro

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val kappa = flag "--kappa"
    (Plain ([], "A comma-separated list of additional exponents to use for calculating awpd."))
  val include_pendant = flag "--include-pendant"
    (Plain (false, "Consider pendant branch length in diversity calculations."))

  method specl =
    super_mass#specl
    @ super_tabular#specl
    @ [
      delimited_list_flag kappa;
      toggle_flag include_pendant;
    ]

  method desc = "calculates various alpha diversity metrics of placefiles"
  method usage = "usage: fpd [options] placefile[s]"

  method private placefile_action prl =
    let exponents = fv kappa |> List.map float_of_string
    and criterion = self#criterion
    and include_pendant = fv include_pendant in
    let awpd = awpd_of_placerun ~include_pendant criterion
    and pd = pd_of_placerun ~include_pendant criterion
    and rpd = pd_of_placerun ~include_pendant ~bump:bump_with_root criterion
    and entropy = entropy_of_placerun ~include_pendant criterion in
    prl
      |> List.map
          (fun pr ->
            let pe, qe = entropy pr in
            [pe; qe; pd pr; rpd pr; awpd 1. pr]
            |> (flip List.append (List.map (flip awpd pr) exponents))
            |> List.map (Printf.sprintf "%g")
            |> List.cons (Placerun.get_name pr))
      |> List.cons
          (["placerun"; "phylo_entropy"; "quadratic"; "pd"; "pd_with_root";
            "awpd"]
           @ List.map (Printf.sprintf "awpd_%g") exponents)
      |> self#write_ll_tab

end
