open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let flip f x y = f y x
let compose f g a = f (g a)
let rec take n cs = match cs with
  | [] -> []
  | c::cs -> match n with
      | 0 -> []
      | n -> c :: (take (n-1) cs)

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
      and md = fvo max_dist
      and mr = fvo max_reported
      and ch = self#out_channel in
      let graph = Voronoi.of_gtree gt in
      let snipdist = Voronoi.get_snipdist graph in
      let dist = Voronoi.placement_distance graph ~snipdist
      and best_placement = Pquery.best_place criterion in
      let pq_distances = List.map
        (fun pr -> dist (best_placement pr), pr)
        (Placerun.get_pqueries pr)
      in
      let sorted_distances = List.sort
        (compose (compose (~-)) compare)
        pq_distances
      in
      let queried_distances = 
        List.concat (List.map 
                  (fun (dist,pq) -> 
                    (List.map (fun name -> (name, dist)) (Pquery.namel pq)))
                  sorted_distances)
      in 
      let within_limit = match md with
        | Some x -> List.filter (fun (_,dist) -> dist > x) queried_distances
        | None -> queried_distances
      in
      let trimmed = match mr with
        | Some x -> take x within_limit
        | None -> within_limit
      in List.iter 
           (fun (name,dist) -> Csv.save_out ch [[name; Printf.sprintf "%1.6f" dist]])
           trimmed

    | _ -> failwith "diplac takes exactly one placefile"

end
