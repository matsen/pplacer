open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  method desc = "checks a reference package"
  method usage = "usage: check -c my.refpkg"

  method action _ =
    Refpkg.check_refpkg self#get_rp
end
