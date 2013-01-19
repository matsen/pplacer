open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val split_csv = flag "--split-csv"
    (Needs_argument ("", "Write out a csv file indicating the source of each read in the combined placefile."))

  method specl =
    super_output#specl
  @ [string_flag split_csv]

  method desc = "merges placefiles together"
  method usage = "usage: merge [options] placefiles"

  method private placefile_action = function
    | [] -> failwith "merge takes at least one placefile (zero given)"
    | prl ->
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".jplace"))
        ()
      in
      begin match fvo split_csv with
        | None -> ()
        | Some fname ->
          let csv_out = open_out fname |> csv_out_channel |> Csv.to_out_obj in
          List.enum prl
          |> Enum.map (fun pr ->
            let pr_name = Placerun.get_name pr in
            Placerun.get_pqueries pr
            |> List.enum
            |> Enum.map (Pquery.namel %> List.enum)
            |> Enum.flatten
            |> Enum.map (fun name -> [name; pr_name]))
          |> Enum.flatten
          |> Enum.iter (Csv.output_record csv_out)
      end;
      let combined = List.reduce (Placerun.combine "") prl in
      self#write_placefile fname combined
end
