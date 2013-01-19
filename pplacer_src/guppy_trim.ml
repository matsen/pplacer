open Subcommand
open Guppy_cmdobjs
open Ppatteries

module I = Mass_map.Indiv

let translate_pendant_pql transm pql =
  List.map
    (let open Placement in
     Pquery.apply_to_place_list
       (List.map
          (fun p -> match IntMap.Exceptionless.find p.location transm with
            | None -> p
            | Some (None, _) -> invalid_arg "translate_pendant_bl"
            | Some (Some location, bl_boost) ->
              {p with location; distal_bl = 0.;
                pendant_bl = bl_boost -. p.distal_bl +. p.pendant_bl})))
    pql

let trim min_mass rewrite_discarded_mass weighting criterion gt pql =
  let mass = pql
    |> Mass_map.Pre.of_pquery_list weighting criterion
    |> Mass_map.Indiv.of_pre
    |> IntMap.map (List.fold_left (fun accum {I.mass} -> accum +. mass) 0.)
  and bl = Gtree.get_bl gt in
  let rec aux mass_above t =
    let open Stree in
    let i = top_id t in
    let mass_above' = mass_above +. IntMap.get i 0. mass in
    match t with
      | Leaf _ when mass_above' >= min_mass -> Some t, IntMap.empty
      | Leaf _ -> None, IntMap.singleton i (None, bl i)
      | Node (_, subtrees) ->
        match List.map (aux mass_above') subtrees
          |> List.split
          |> (Tuple2.map (List.filter_map identity) (List.reduce IntMap.union))
        with
          | [], transm ->
            None,
            IntMap.map (Tuple.Tuple2.map2 ((+.) (bl i))) transm
            |> IntMap.add i (None, bl i)
          | subtrees', transm ->
            Some (node i subtrees'),
            IntMap.map (Tuple.Tuple2.map1 (function None -> Some i | x -> x)) transm
  in
  let st', pre_transm = Gtree.get_stree gt |> aux 0. in
  let pql =
    if not rewrite_discarded_mass then pql
    else translate_pendant_pql pre_transm pql
  in
  let gt', transm = Option.get st'
    |> Gtree.set_stree gt
    |> Newick_gtree.consolidate
  and discarded_reads = RefList.empty () in
  let add_discarded = List.iter (RefList.add discarded_reads) in
  List.filter_map
    (fun pq ->
      Pquery.place_list pq
        |> List.filter_map
            (fun p ->
              if IntMap.mem (Placement.location p) transm
              then Some p else None)
        |> junction
            List.is_empty
            (fun _ -> add_discarded (Pquery.namel pq); None)
            (fun place_list -> Some {pq with Pquery.place_list}))
    pql
  |> Pquery.translate_pql transm
  |> List.map Pquery.renormalize_log_like
  |> Placerun.make ~transm gt' "",
  discarded_reads

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val min_path_mass = flag "--min-path-mass"
    (Formatted (0.001, "The minimum mass which must be on the path to a leaf to keep it. default: %g"))
  val discarded = flag "--discarded"
    (Needs_argument ("", "A file to write discarded pqueries to."))
  val rewrite_discarded_mass = flag "--rewrite-discarded-mass"
    (Plain (false, "Move placements which were on discarded leaves to the nearest non-discarded node."))

  method specl =
    super_mass#specl
  @ super_output#specl
  @ [
    float_flag min_path_mass;
    string_flag discarded;
    toggle_flag rewrite_discarded_mass;
  ]

  method desc = "trims placefiles down to only containing an informative subset of the mass"
  method usage = "usage: trim [options] placefile[s]"

  method private placefile_action prl =
    let gt = Mokaphy_common.list_get_same_tree prl
      |> Newick_gtree.add_zero_root_bl
    and weighting, criterion = self#mass_opts in
    let pr, discarded_reads = prl
    |> List.map (Placerun.unitize %> Placerun.get_pqueries)
    |> List.flatten
    |> trim
        (fv min_path_mass)
        (fv rewrite_discarded_mass)
        weighting
        criterion
        gt
    in
    self#write_placefile (self#single_file ()) pr;
    match fvo discarded with
      | Some fname -> RefList.enum discarded_reads |> File.write_lines fname
      | None when RefList.is_empty discarded_reads -> ()
      | None ->
        dprint "Discarded reads:\n";
        RefList.iter (dprintf "%s\n") discarded_reads

end
