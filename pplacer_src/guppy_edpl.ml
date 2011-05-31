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

  method private make_dist_fun prl =
    let _, weighting, criterion = self#mass_opts in
    Pquery_distances.dist_fun_of_weight
      weighting
      criterion
      (Edge_rdist.build_ca_info (Mokaphy_common.list_get_same_tree prl))

    method private placefile_action prl =
      let df = self#make_dist_fun prl in
      List.iter
        (fun pr ->
          let ch = open_out ((Placerun.get_name pr)^".edpl") in
          List.iter
            (fun pq ->
              let edpl = df pq pq in
              List.iter
                (fun name -> Printf.fprintf ch "%s\t%g\n" name edpl)
                pq.Pquery.namel)
            pr.Placerun.pqueries;
          close_out ch;
        )
        prl
end
