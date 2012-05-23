open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit placefile_cmd () as super_placefile

  val unitize = flag "--unitize"
    (Plain (false, "Make total unit mass per placerun by multiplying with a scalar."))
  val transform = flag "--transform"
    (Plain ("", "A transform to apply to the read multiplicities before calculating. \
    Options are 'log', 'unit', 'asinh', and 'no_trans'. Default is no transform."))
  val copy_number = flag "--copy-number"
    (Needs_argument ("", "Rescale placements by the copy number, given a CSV file mapping from the names of the \
                          leaves on the reference tree to their copy numbers."))

  method specl =
    super_output#specl
  @ [
    toggle_flag unitize;
    string_flag transform;
    string_flag copy_number;
  ]

  method desc = "Multi-Filter and Transform placefiles"
  method usage = "usage: mft [options] placefile[s]"

  method private placefile_action prl =
    let prl = prl
    |> (match fv transform with
        | "" -> identity
        | transform ->
          List.map (Mass_map.transform_of_str transform |> Placerun.transform))
    |> (match fvo copy_number with
        | None -> identity
        | Some infile ->
          Csv.load infile
            |> List.map
                (function
                  | [a; b] -> a, float_of_string b
                  | _ -> failwith "malformed copy number csv file")
            |> StringMap.of_pairlist
            |> Indep_contrasts.of_criterion_map Placement.ml_ratio
            |> List.map)
    |> if not (fv unitize) then identity else List.map Placerun.unitize
    in
    match self#out_file_or_dir () with
      | Directory (dir, prefix) ->
        List.iter
          (fun pr ->
            let fname = Filename.concat
              dir
              (prefix ^ pr.Placerun.name ^ ".jplace")
            in
            self#write_placefile fname pr)
          prl
      | File fname ->
        prl
          |> List.map Placerun.get_pqueries
          |> List.flatten
          |> Placerun.make (Mokaphy_common.list_get_same_tree prl) ""
          |> self#write_placefile fname
      | Unspecified -> ()

end
