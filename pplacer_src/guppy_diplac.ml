open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let flip f x y = f y x
let compose f g a = f (g a)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl =
    super_mass#specl
    @ super_output#specl

  method desc = "find the most DIstant PLACements from the leaves"
  method usage = "usage: diplac [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let _, _, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
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
      List.iter
        (fun (dist, pq) ->
          let namel = List.map
            (fun name -> [name; Printf.sprintf "%1.6f" dist])
            (Pquery.namel pq)
          in
          Csv.save_out ch namel)
        sorted_distances

    | _ -> failwith "diplac takes exactly one placefile"

end
