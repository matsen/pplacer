open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile

  method specl = []

  method desc =
"converts old-style .place files to .jplace placement files"
  method usage = "usage: to_json placefile[s]"

  method private placefile_action prl =
    List.iter
      (fun pr ->
        let out_name = (pr.Placerun.name ^ ".jplace") in
        self#write_placefile out_name pr)
      prl
end
