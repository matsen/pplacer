open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~weighting_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile

  val first_only = flag "--first-only"
    (Plain (false, "Only print the first name for each pquery."))

  method desc =
    "calculates the EDPL uncertainty values for a collection of pqueries"
  method usage = "usage: edpl [options] placefile[s]"

  method specl =
    super_mass#specl
    @ [
      toggle_flag first_only
    ]

    method private placefile_action prl =
      let criterion = self#criterion in
      let select_fn =
        if fv first_only then fun x -> [List.hd x]
        else fun x -> x
      in
      List.iter
        (fun pr ->
          let dm = Edge_rdist.build_pairwise_dist pr.Placerun.ref_tree in
          let edpl = Edpl.of_pquery criterion dm in
          let ch = open_out ((Placerun.get_name pr)^".edpl") in
          List.iter
            (fun pq ->
              List.iter
                (fun name -> Printf.fprintf ch "%s\t%g\n" name (edpl pq))
                (Pquery.namel pq |> select_fn))
            pr.Placerun.pqueries;
          close_out ch;
        )
        prl
end
