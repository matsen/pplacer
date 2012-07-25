open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd ~default_to_csv:true () as super_tabular
  inherit mass_cmd () as super_mass

  method specl =
    super_tabular#specl
  @ super_mass#specl

  method desc =
"turns a placefile into a csv file"
  method usage = "usage: to_csv [options] placefile[s]"

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts
    and format = Printf.sprintf "%g" in
    flip List.map prl (fun pr ->
      let pr_name = Placerun.get_name pr in
      Placerun.get_pqueries pr
      |> List.map (fun pq ->
        (match weighting with
         | Mass_map.Spread -> Pquery.place_list pq
         | Mass_map.Point -> [Pquery.best_place criterion pq])
        |> List.cartesian_product (Pquery.namlom pq)
        |> List.map (fun ((name, weight), pq) ->
          [pr_name; name; format weight] @ (Placement.to_csv_strl pq)))
      |> List.flatten)
    |> List.flatten
    |> List.cons
        ["origin"; "name"; "multiplicity"; "edge_num"; "like_weight_ratio";
         "post_prob"; "likelihood"; "marginal_like"; "distal_length";
         "pendant_length"; "classification"; "map_ratio"; "map_overlap";
         "map_divergence_ratio"]
    |> self#write_ll_tab

end
