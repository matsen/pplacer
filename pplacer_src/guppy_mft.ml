open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit placefile_cmd () as super_placefile

  val average = flag "--average"
    (Plain (false, "Give equal weight to each pquery per-placerun."))

  method specl = super_output#specl @ [toggle_flag average]

  method desc = "filter and transform placefiles"
  method usage = "usage: mft [options] placefile[s]"

  method private placefile_action prl =
    let prl =
      if not (fv average) then prl else
        List.map
          (fun pr ->
            let pql = Placerun.get_pqueries pr in
            let totmass = List.fold_left
              (fun accum pq -> accum +. ((float_of_int (Pquery.multiplicity pq)) *. (Pquery.pq_mass pq)))
              0.
              pql
            in
            let pql' = List.map
              (fun pq ->
                Pquery.set_pq_mass ((Pquery.pq_mass pq) /. totmass) pq)
              pql
            in
            Placerun.set_pqueries pr pql')
          prl
    in
    match self#out_file_or_dir () with
      | Directory (dir, prefix) ->
        List.iter
          (fun pr ->
            let fname = Filename.concat
              dir
              (prefix ^ pr.Placerun.name ^ ".jplace")
            in
            self#write_placefile "guppy mft" fname pr)
          prl
      | File fname ->
        let pql = Base.map_and_flatten Placerun.get_pqueries prl in
        let pr = Placerun.make (Mokaphy_common.list_get_same_tree prl) "" pql in
        self#write_placefile "guppy mft" fname pr
      | Unspecified -> ()

end
