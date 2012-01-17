open Subcommand
open Guppy_cmdobjs
open Ppatteries

(* Note that this version of rounded placements does not take the likelihood
 * weight ratio into account. However, we do preserve the order of appearance of
 * edges in the rounded pquery, which does maintain some of that information.
 * Furthermore, branch lengths must be similar.
 *)
type rounded_placement =
  {
    location: int;
    distal_bl: float;
    pendant_bl: float;
  }

let rounded_placement_of_placement multiplier p =
  let round = match multiplier with
    | Some mult -> fun x -> float_of_int (round (mult *. x))
    | None -> fun x -> x
  in
  {
    location = p.Placement.location;
    distal_bl = round p.Placement.distal_bl;
    pendant_bl = round p.Placement.pendant_bl;
  }

type rounded_pquery = rounded_placement list

(* compare first by descending ml ratio, then arbitrary *)
let compare_by_ml_ratio x y =
  match compare x.Placement.ml_ratio y.Placement.ml_ratio with
  | 0 -> compare x y
  | ml_cmp -> - ml_cmp

(* Round after filtering the ones that don't make the ML ratio cutoff.
 * We sort here to make sure that they are in ml_ratio descending order. *)
let rounded_pquery_of_pquery cutoff multiplier pq =
  List.map
    (rounded_placement_of_placement multiplier)
    (List.sort
      compare_by_ml_ratio
      (List.filter
        (fun p -> p.Placement.ml_ratio >= cutoff)
        pq.Pquery.place_list))

module RPQMap =
  Map.Make
    (struct
      type t = rounded_pquery
      let compare = Pervasives.compare
    end)

let add_listly k v m =
  if RPQMap.mem k m then RPQMap.add k (v::(RPQMap.find k m)) m
  else RPQMap.add k [v] m

(* given a list of pqueries, we round them, then cluster the pqueries which are
 * identical after the rounding step. *)
let round_pquery_list cutoff sig_figs pql =
  assert(sig_figs > 0);
  let multiplier = match sig_figs with
    | 0 -> None
    | f -> Some (int_pow 10. f)
  in
  (* collect placements together using a map *)
  let m =
    List.fold_left
      (fun m pq ->
        add_listly (rounded_pquery_of_pquery cutoff multiplier pq) pq m)
      RPQMap.empty
      pql
  in
  (* take the first placement in the collected list, and give it all of the
   * names associated with the other pqueries *)
  List.map
    (fun pql ->
      List.map Pquery.namlom pql
        |> List.flatten
        |> Pquery.set_namlom (List.hd pql))
    (RPQMap.fold (fun _ v l -> (List.rev v)::l) m []) (* the clustered ones *)

let round_placerun cutoff sig_figs out_name pr =
  {pr with
    Placerun.pqueries =
      round_pquery_list cutoff sig_figs (pr.Placerun.pqueries);
    name = out_name;}

(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~show_fname:false () as super_output
  inherit placefile_cmd () as super_placefile

  val sig_figs = flag "--sig-figs"
    (Formatted (3, "Set the number of significant figures used for rounding. Specify 0 for no rounding. Default: %d."))
  val cutoff = flag "--cutoff"
    (Formatted (0.01, "Set the rounding inclusion cutoff for the ML weight ratio. Default: %g."))

  method specl = super_output#specl @ [
    int_flag sig_figs;
    float_flag cutoff;
  ]

  method desc =
"clusters the placements by rounding branch lengths"
  method usage = "usage: round [options] placefile[s]"

  method private placefile_action prl =
    let prefix = self#single_prefix ()
    and round_placerun = round_placerun (fv cutoff) (fv sig_figs) in
    List.iter
      (fun pr ->
        let out_name = (prefix^(pr.Placerun.name)) in
        self#write_placefile
          "guppy round"
          (out_name^".jplace")
          (round_placerun out_name pr))
      prl
end
