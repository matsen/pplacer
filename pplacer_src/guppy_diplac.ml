open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val max_dist = flag "--min-distance"
    (Needs_argument ("min distance", "Specify the minimum distance to leaves to report"))
  val max_reported = flag "--max-matches"
    (Needs_argument ("N", "Only report the deepest N placements"))

  method specl = super_mass#specl @ [
    float_flag max_dist;
    int_flag max_reported
  ] @ super_output#specl

  method desc = "find the most DIstant PLACements from the leaves"
  method usage = "usage: diplac [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let _, _, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
      and ch = self#out_channel |> csv_out_channel |> Csv.to_out_obj in
      let graph = Voronoi.of_gtree gt in
      let snipdist = Voronoi.get_snipdist graph in
      let dist = Voronoi.placement_distance graph ~snipdist
      and best_placement = Pquery.best_place criterion in
      Placerun.get_pqueries pr
      |> List.map
        (fun pr -> dist (best_placement pr), pr)
      |> List.sort ~cmp:(flip compare)
      |> List.enum
      |> Enum.map
          (fun (dist, pq) ->
            Pquery.namel pq
            |> List.enum
            |> (dist |> curry identity |> Enum.map))
      |> Enum.flatten
      |> (match fvo max_dist with
          | Some max_dist -> Enum.filter (fun (dist, _) -> dist > max_dist)
          | None -> identity)
      |> (match fvo max_reported with
          | Some n -> Enum.take n
          | None -> identity)
      |> Enum.iter
          (fun (dist, name) ->
            Csv.output_all ch [[name; Printf.sprintf "%1.6f" dist]])

    | _ -> failwith "diplac takes exactly one placefile"

end
