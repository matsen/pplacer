open Subcommand
open Guppy_cmdobjs

let demulti_pquery_list pql =
  List.flatten
    (List.map
      (fun pq ->
        List.map
          (fun name -> {pq with Pquery.namel = [name]})
          pq.Pquery.namel)
      pql)

let demulti_placerun out_name pr =
  {pr with
    Placerun.pqueries =
      demulti_pquery_list pr.Placerun.pqueries;
    name = out_name;}

(* UI-related *)

class cmd () =
object
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit placefile_cmd () as super_placefile

  method desc =
"splits apart placements with multiplicity, undoing a round procedure"
  method usage = "usage: demulti [options] placefile[s]"

  method private placefile_action prl =
    let out_prefix = fv out_prefix in
    List.iter
      (fun pr ->
        let out_name = (out_prefix^(pr.Placerun.name)) in
        Placerun_io.to_json_file
          "guppy demulti"
          (out_name^".json")
          (demulti_placerun out_name pr))
      prl
end
