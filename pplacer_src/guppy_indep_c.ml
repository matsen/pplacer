open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit tabular_cmd () as super_tabular

  val copy_number = flag "--leaf-values"
    (Needs_argument ("leav values", "Name of CSV file giving values for the leaves of the tree."))

  method specl =
    super_mass#specl
  @ super_tabular#specl
  @ [string_flag copy_number]

  method desc =
"calculates the independent contrasts of pqueries"
  method usage = "usage: indep_c [options] placefile[s]"

  method private placefile_action prl =
    let leaf_copy_map = fv copy_number
      |> Csv.load
      |> List.map
          (function
           | [a; b] -> a, float_of_string b
           | _ -> failwith "malformed copy number csv file")
      |> StringMap.of_pairlist
    and criterion = self#criterion
    and format = Printf.sprintf "%g" in
    flip List.map prl (fun pr ->
      Indep_contrasts.of_criterion_map criterion leaf_copy_map pr
      |> List.map (fun (pq, x) ->
        List.map (flip List.cons [format x]) (Pquery.namel pq))
      |> List.flatten)
    |> List.flatten
    |> List.cons ["sequence"; "x"]
    |> self#write_ll_tab

end
