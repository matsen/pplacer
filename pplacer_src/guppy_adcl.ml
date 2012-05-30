open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd ~default_to_csv:true () as super_tabular

  val max_dist = flag "--min-distance"
    (Needs_argument ("min distance", "Specify the minimum distance to leaves to report"))
  val max_reported = flag "--max-matches"
    (Needs_argument ("N", "Only report the deepest N placements"))
  val include_pendant = flag "--include-pendant"
    (Plain (false, "Include pendant branch lengths in distance calculations."))
  val no_collapse = flag "--no-collapse"
    (Plain (false, "List all of the names per pquery instead of just one."))

  method specl = super_mass#specl @ [
    float_flag max_dist;
    int_flag max_reported;
    toggle_flag include_pendant;
    toggle_flag no_collapse;
  ] @ super_tabular#specl

  method desc = "calculates ADCL for each pquery in a placefile"
  method usage = "usage: adcl [options] placefile"

  method private dist dist_fn p =
    dist_fn p
    |> (if fv include_pendant then (+.) (Placement.pendant_bl p) else identity)

  method private placefile_action = function
    | [pr] ->
      let criterion = self#criterion
      and collapse_fn =
        if fv no_collapse then
          Pquery.namlom
        else
          fun pq -> [Pquery.name pq, Pquery.multiplicity pq]
      and gt = Placerun.get_ref_tree pr in
      let graph = Voronoi.of_gtree gt in
      let snipdist = Voronoi.get_snipdist graph in
      let dist = Voronoi.placement_distance graph ~snipdist
      and best_placement = Pquery.best_place criterion in
      Placerun.get_pqueries pr
      |> List.map
          (fun pq ->
            let distance = self#dist dist (best_placement pq) in
            List.map (curry identity distance) (collapse_fn pq))
      |> List.flatten
      |> List.sort (flip compare)
      |> List.enum
      |> (match fvo max_dist with
          | Some max_dist -> Enum.filter (fun (dist, _) -> dist > max_dist)
          | None -> identity)
      |> (match fvo max_reported with
          | Some n -> Enum.take n
          | None -> identity)
      |> Enum.map (fun (dist, (name, multiplicity)) ->
          [name; Printf.sprintf "%1.6f" dist; Printf.sprintf "%g" multiplicity])
      |> List.of_enum
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "adcl takes exactly one placefile (%d given)"
      |> failwith

end
