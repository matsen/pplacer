open Subcommand
open Guppy_cmdobjs
open Ppatteries

let pr_to_map transform pr =
  List.fold_left
    (fun accum pq ->
      let v = transform pq in
      List.fold_left (fun a k -> StringMap.add k v a) accum (Pquery.namel pq))
    StringMap.empty
    (Placerun.get_pqueries pr)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val include_pendant = flag "--include-pendant"
    (Plain (false, "Include pendant branch lengths in distance calculations."))

  method specl =
    super_mass#specl
  @ super_tabular#specl
  @ [toggle_flag include_pendant]

  method desc = "finds the error between two placefiles"
  method usage = "usage: error [options] experimental.jplace expected.jplace"

  method private placefile_action = function
    | [experimental; expected] ->
      let criterion = self#criterion
      and include_pendant = fv include_pendant
      and gt = Placerun.get_same_tree experimental expected in
      let pair_dist = Edge_rdist.build_pairwise_dist gt
        |> Edge_rdist.find_pairwise_dist
      in
      let placement_distance p1 p2 =
        let open Placement in
        pair_dist p1.location p1.distal_bl p2.location p2.distal_bl
          +. (if include_pendant then p1.pendant_bl +. p2.pendant_bl else 0.)
      in
      StringMap.merge
        (fun _ po plo -> match po, plo with
          | Some p, Some pl ->
            List.map (placement_distance p &&& criterion) pl
            |> Edpl.weighted_average
            |> (fun x -> Some x)
          | _, _ -> None)
        (pr_to_map (Pquery.best_place criterion) expected)
        (pr_to_map Pquery.place_list experimental)
      |> StringMap.enum
      |> Enum.map (fun (a, b) -> [a; Printf.sprintf "%g" b])
      |> List.of_enum
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "error takes exactly two placefiles (%d given)"
      |> failwith

end
