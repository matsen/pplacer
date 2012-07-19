open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

let of_refpkg rp =
  let gt = Refpkg.get_ref_tree rp
  and td = Refpkg.get_taxonomy rp in
  let st = Gtree.get_stree gt
  and top_id = Gtree.top_id gt in
  rank_tax_map_of_refpkg rp
    |> IntMap.enum
    |> Enum.map
        (fun (rank, taxmap) ->
          let sizemim, cutsetim = build_sizemim_and_cutsetim (taxmap, st) in
          let top_sizem = IntMap.find top_id sizemim in
          let cutsetim = IntMap.add (Stree.top_id st) ColorSet.empty cutsetim in
          let unconvex_colors = IntMap.fold
            (fun _ colors unconvex ->
              if ColorSet.cardinal colors < 2 then unconvex else
                ColorSet.union unconvex colors)
            cutsetim
            ColorSet.empty
          and all_colors = ColorMap.keys top_sizem |> ColorSet.of_enum
          and rank_name = Tax_taxonomy.get_rank_name td rank in
          ColorSet.diff all_colors unconvex_colors
            |> ColorSet.enum
            |> Enum.map (fun c -> rank_name, c, ColorMap.find c top_sizem))
    |> Enum.flatten


class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit tabular_cmd ~default_to_csv:true () as super_tabular

  method specl =
    super_refpkg#specl
  @ super_tabular#specl

  method desc = "determines convex tax_ids per-rank in a refpkg"
  method usage = "usage: convex_taxids -c my.refpkg"

  method action _ =
    of_refpkg self#get_rp
      |> Enum.map (fun (a, b, c) -> [a; Tax_id.to_string b; string_of_int c])
      |> List.of_enum
      |> List.cons ["rank"; "tax_id"; "leaf_count"]
      |> self#write_ll_tab

end
