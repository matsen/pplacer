open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
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
      let _, _, criterion = self#mass_opts in
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
                (select_fn pq.Pquery.namel))
            pr.Placerun.pqueries;
          close_out ch;
        )
        prl
end
