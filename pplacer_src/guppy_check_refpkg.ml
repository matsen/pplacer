open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd () as super_refpkg

  method desc = "check a reference package"
  method usage = "usage: check_refpkg -c <refpkg>"

  method action _ =
    match self#get_rpo with
      | None -> ()
      | Some rp -> Refpkg.check_refpkg rp
end
