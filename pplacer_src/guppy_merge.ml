open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method desc = "merges placefiles together"
  method usage = "usage: merge [options] placefiles"

  method private placefile_action = function
    | [] -> ()
    | prl ->
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".jplace"))
        ()
      in
      let combined = List.fold_left
        (Placerun.combine "")
        (List.hd prl)
        (List.tl prl)
      in
      self#write_placefile "guppy merge" fname combined
end
