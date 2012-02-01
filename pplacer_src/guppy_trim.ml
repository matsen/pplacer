open Subcommand
open Guppy_cmdobjs
open Ppatteries

module I = Mass_map.Indiv

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val min_path_mass = flag "--min-path-mass"
    (Formatted (0.001, "The minimum mass which must be on the path to a leaf to keep it. default: %g"))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [float_flag min_path_mass]

  method desc = "trims placefiles down to only containing an informative subset of the mass"
  method usage = "usage: trim [options] placefile[s]"

  method private placefile_action prl =
    let gt = Mokaphy_common.list_get_same_tree prl
    and weighting, criterion = self#mass_opts in
    let pql = List.map (Placerun.unitize |- Placerun.get_pqueries) prl
      |> List.flatten
    in
    let mass = pql
      |> Mass_map.Pre.of_pquery_list weighting criterion
      |> Mass_map.Indiv.of_pre
      |> IntMap.map (List.fold_left (fun accum {I.mass} -> accum +. mass) 0.)
    and min_mass = fv min_path_mass in
    let rec aux mass_above t =
      let open Stree in
      let i = top_id t in
      let mass_above' = mass_above +. IntMap.get i 0. mass in
      match t with
        | Leaf _ when mass_above' >= min_mass -> Some t
        | Leaf _ -> None
        | Node (_, subtrees) ->
          List.filter_map (aux mass_above') subtrees
          |> junction List.is_empty (const None) (node i |- some)
    in
    let gt', transm = Gtree.get_stree gt
      |> aux 0.
      |> Option.get
      |> Gtree.set_stree gt
      |> Newick_gtree.consolidate
    in
    List.filter_map
      (fun pq ->
        Pquery.place_list pq
        |> List.filter_map
            (fun p ->
              if IntMap.mem (Placement.location p) transm
              then Some p else None)
        |> junction
            List.is_empty
            (const None)
            (fun place_list -> Some {pq with Pquery.place_list}))
      pql
    |> Pquery.translate_pql transm
    |> List.map Pquery.renormalize_log_like
    |> Placerun.make gt' ""
    |> self#write_placefile (self#single_file ())
end
