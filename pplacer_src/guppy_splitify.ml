open Subcommand
open Guppy_cmdobjs

open Ppatteries

(* *** splitify *** *)

let fal_to_strll fal =
  List.map
    (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
    fal

let save_out_named_fal ch fal =
  csv_out_channel ch
    |> Csv.to_out_obj
    |> flip Csv.output_all (fal_to_strll fal)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit splitify_cmd () as super_splitify

  val! epsilon = flag "--epsilon"
    (Needs_argument ("", "The epsilon to use to determine if a split matrix's column \
                          is constant for filtering. default: no filtering"))

  method specl =
    super_output#specl
  @ super_mass#specl
  @ super_splitify#specl

  method desc =
"writes out differences of masses for the splits of the tree"
  method usage = "usage: splitify [options] placefile(s)"

  method private maybe_filter_constant_columns =
    match fvo epsilon with
    | None -> identity
    | Some _ -> self#filter_constant_columns |- Tuple3.first

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts in
    let data = List.map (self#splitify_placerun weighting criterion) prl
    and names = (List.map Placerun.get_name prl) in
    let data' = self#filter_rep_edges prl data
      |> Tuple3.first
      |> self#maybe_filter_constant_columns
    in
    save_out_named_fal
      self#out_channel
      (List.combine names data')

end
