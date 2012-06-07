open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method specl =
    super_mass#specl
  @ super_tabular#specl

  method desc = "calculates phylogenetic rarefaction curves"
  method usage = "usage: rarefact [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let criterion = self#criterion in
      let fmt = Printf.sprintf "%g" in
      Enum.combine
        (Rarefaction.of_placerun criterion pr,
         Rarefaction.variance_of_placerun criterion pr)
      |> Enum.map
          (fun ((k, um, rm, qm), (_, uv, rv)) ->
            [string_of_int k; fmt um; fmt rm; fmt qm; fmt uv; fmt rv])
      |> List.of_enum
      |> List.cons
          ["k"; "unrooted_mean"; "rooted_mean"; "quadratic_mean";
           "unrooted_variance"; "rooted_variance"]
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "rarefact takes exactly one placefile (%d given)"
      |> failwith

end
