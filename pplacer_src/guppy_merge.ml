open Subcommand
open Guppy_cmdobjs

class cmd () =
object
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile

  val outfile = flag "-o"
    (Plain ("", "Output file. Default is derived from the input filenames."))

  method specl = [string_flag outfile]

  method desc = "merges placefiles together"
  method usage = "usage: merge [options] placefiles"

  method private placefile_action = function
    | [] -> ()
    | prl ->
      let fname = match fv outfile with
        | "" -> (Mokaphy_common.cat_names prl) ^ ".json"
        | s -> s
      in
      let combined = List.fold_left
        (Placerun.combine "")
        (List.hd prl)
        (List.tl prl)
      in
      Placerun_io.to_json_file "guppy merge" fname combined
end
