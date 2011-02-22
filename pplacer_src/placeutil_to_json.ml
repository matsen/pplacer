open Subcommand

class cmd () =
object
  inherit subcommand () as super

  method specl = []

  method desc = "converts .place files to .place.json files."
  method usage = "usage: to_json placefile[s]"

  method action fnamel =
    List.iter begin
      fun fname ->
        let pr = Placerun_io.of_file fname in
        let out_name = (fname ^ ".json") in
        Placerun_io.to_json_file
          (String.concat " " ("placeutil"::fnamel))
          out_name
          pr
    end fnamel
end
