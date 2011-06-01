open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile

  method desc =
    "calculates the EDPL value for a collection of pqueries (assumed to have same ref tree)"
  method usage = "usage: edpl [options] placefiles"

  method specl =
    super_mass#specl

    method private placefile_action prl =
      let _, _, criterion = self#mass_opts in
      List.iter
        (fun pr ->
          let dm = Edge_rdist.build_pairwise_dist pr.Placerun.ref_tree in
          let edpl = Edpl.of_pquery criterion dm in
          let ch = open_out ((Placerun.get_name pr)^".edpl") in
          List.iter
            (fun pq ->
              List.iter
                (fun name -> Printf.fprintf ch "%s\t%g\n" name (edpl pq))
                pq.Pquery.namel)
            pr.Placerun.pqueries;
          close_out ch;
        )
        prl
end
