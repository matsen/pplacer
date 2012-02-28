open Ppatteries
open Subcommand
open Guppy_cmdobjs
open Placerun


class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile

  method specl = []

  method desc =
"checks placefiles for common problems"
  method usage = "usage: check [options] placefile[s]"

  method private check_locations pr =
    let bl = Placerun.get_ref_tree pr |> Gtree.get_bl in
    Placerun.get_pqueries pr
    |> List.iter (fun pq ->
      let open Placement in
      let name = Pquery.name pq in
      List.iter
        (fun p ->
          if p.distal_bl > bl p.location then
            dprintf "invalid placement location in %s: %g > %g\n"
              name p.distal_bl (bl p.location);
          if p.distal_bl < 0. then
            dprintf "invalid placement location in %s: %g < 0\n"
              name p.distal_bl)
      (Pquery.place_list pq))

  method private placefile_action prl =
    List.iter self#check_locations prl

end
