open Subcommand

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
  inherit Guppy_cmdobjs.out_prefix_cmd () as out_prefix_super

  method desc = "splits apart placements with multiplicity, effectively undoing a round procedure."
  method usage = "usage: demulti [options] placefile[s]"

  method action fnamel =
    let out_prefix = fv out_prefix in
    List.iter
      (fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (out_prefix^(pr.Placerun.name)) in
        Placerun_io.to_file
          (String.concat " " ("placeutil"::fnamel))
          (out_name^".place")
          (demulti_placerun out_name pr))
      fnamel
end
