open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val first_only = flag "--first-only"
    (Plain (false, "Only print the first name for each pquery."))

  method desc =
    "calculates the EDPL uncertainty values for a collection of pqueries"
  method usage = "usage: edpl [options] placefile"

  method specl =
    super_tabular#specl
  @ super_mass#specl
  @ [toggle_flag first_only]

  method private placefile_action = function
    | [pr] ->
      let select_fn = if fv first_only then fun x -> [List.hd x] else identity
      and criterion = self#criterion
      and dm = Placerun.get_ref_tree pr |> Edge_rdist.build_pairwise_dist in
      let edpl = Edpl.of_pquery criterion dm in
      List.map
        (fun pq ->
          List.map
            (fun name -> [name; edpl pq |> Printf.sprintf "%g"])
            (Pquery.namel pq |> select_fn))
        (Placerun.get_pqueries pr)
      |> List.flatten
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "edpl takes exactly one placefile (%d given)"
      |> failwith

end
