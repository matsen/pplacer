open Guppy_cmdobjs
open Subcommand
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit tabular_cmd () as super_tabular
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  method specl =
    super_tabular#specl
  @ super_mass#specl

  method desc =
"prints out a pairwise distance matrix between placements"
  method usage = "usage: placemat [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let dist = Placerun.get_ref_tree pr
        |> Edge_rdist.build_pairwise_dist
        |> Edge_rdist.find_pairwise_dist
      and weighting, criterion = self#mass_opts in
      let placement_dist p1 p2 =
        dist
          (Placement.location p1)
          (Placement.distal_bl p1)
          (Placement.location p2)
          (Placement.distal_bl p2)
      in
      let pquery_dist = match weighting with
        | Mass_map.Point -> fun pq1 pq2 ->
          placement_dist
            (Pquery.best_place criterion pq1)
            (Pquery.best_place criterion pq2)
        | Mass_map.Spread -> fun pq1 pq2 ->
          List.cartesian_product
            (Pquery.place_list pq1)
            (Pquery.place_list pq2)
          |> List.map
              (fun (p1, p2) -> placement_dist p1 p2 *. criterion p1 *. criterion p2)
          |> List.fsum
      in
      let pqa = Array.of_list (Placerun.get_pqueries pr) in
      Uptri.init
        (Array.length pqa)
        (fun i j ->
          let pq1 = pqa.(i) and pq2 = pqa.(j) in
          [Pquery.name pq1;
           Pquery.name pq2;
           Printf.sprintf "%g" (pquery_dist pq1 pq2)])
      |> Uptri.to_array
      |> Array.to_list
      |> List.cons ["name1"; "name2"; "distance"]
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "placemat takes exactly one placefile (%d given)"
      |> failwith

end
