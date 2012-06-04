open Subcommand
open Guppy_cmdobjs
open Ppatteries

let of_pql criterion pql =
  let pqa = Array.of_list pql in
  let name_map =
    Enum.combine
      (Array.enum pqa |> Enum.map Pquery.name, Enum.range 0)
    |> StringMap.of_enum
  in
  let count = StringMap.cardinal name_map in
  let arrays = Array.init count (fun _ -> [||]) in
  let insert idx v =
    arrays.(idx) <- Array.append arrays.(idx) [|v|]
  in
  Mass_overlap.of_pql criterion pql
    |> Enum.iter (fun (n1, n2, v) ->
      let i = StringMap.find n1 name_map
      and j = StringMap.find n2 name_map in
      insert i (j, v);
      insert j (i, v));
  Mcl.mcl arrays
    |> Array.map (Array.map (Array.get pqa))

let islands_of_pql criterion pql =
  of_pql criterion pql
    |> Array.enum
    |> Enum.map (fun pqa -> IntSet.empty, Array.to_list pqa)
    |> List.of_enum

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular
  inherit mass_cmd ~point_choice_allowed:false () as super_mass

  method specl =
    super_tabular#specl
  @ super_mass#specl

  method desc = "cluster pqueries"
  method usage = "usage: mcl [options] placefile"

  method private placefile_action = function
    | [pr] ->
      Placerun.get_pqueries pr
      |> of_pql self#criterion
      |> Array.enum
      |> Enum.mapi
          (fun e arr ->
            Array.enum arr
            |> Enum.map (fun pq -> [Pquery.name pq; string_of_int e]))
      |> Enum.flatten
      |> List.of_enum
      |> List.cons ["pquery"; "cluster"]
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "mcl takes exactly one placefile (%d given)"
      |> failwith

end
