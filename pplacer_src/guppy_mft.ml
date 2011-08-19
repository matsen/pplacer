open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit placefile_cmd () as super_placefile

  val average = flag "--unitize"
    (Plain (false, "Make total unit mass per placerun by multiplying with a scalar."))

  method specl = super_output#specl @ [toggle_flag average]

  method desc = "filter and transform placefiles"
  method usage = "usage: mft [options] placefile[s]"

  method private placefile_action prl =
    let prl =
      if not (fv average) then prl else
        List.map
          (fun pr ->
            let tot_mass = Placerun.get_pqueries pr
              |> Pquery.total_multiplicity
            in
            Placerun.get_pqueries pr
              |> List.map
                  (fun pq ->
                    Pquery.multiplicity pq /. tot_mass |> Pquery.set_mass pq)
              |> Placerun.set_pqueries pr)
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
        prl
          |> List.map Placerun.get_pqueries
          |> List.flatten
          |> Placerun.make (Mokaphy_common.list_get_same_tree prl) ""
          |> self#write_placefile "guppy mft" fname
      | Unspecified -> ()

end
