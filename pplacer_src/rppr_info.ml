open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit tabular_cmd () as super_tabular

  method specl = super_refpkg#specl @ super_tabular#specl

  method desc = "gives information about a reference package"
  method usage = "usage: info -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp
    and td = Refpkg.get_taxonomy rp in
    let st = gt.Gtree.stree in
    rank_tax_map_of_refpkg rp
      |> IntMap.enum
      |> Enum.map
          (fun (rank, taxmap) ->
            let _, cutsetim = build_sizemim_and_cutsetim (taxmap, st) in
            let cutsetim = IntMap.add (Stree.top_id st) ColorSet.empty cutsetim in
            let all_colors, unconvex_colors = IntMap.fold
              (fun _ colors (all, unconvex) ->
                ColorSet.union all colors,
                if ColorSet.cardinal colors < 2 then unconvex else
                  ColorSet.union unconvex colors)
              cutsetim
              (ColorSet.empty, ColorSet.empty)
            and max_bad, tot_bad = badness cutsetim in
            (Tax_taxonomy.get_rank_name td rank) ::
              (List.map
                 string_of_int
                 [ColorSet.cardinal all_colors;
                  ColorSet.cardinal unconvex_colors;
                  max_bad;
                  tot_bad]))
      |> List.of_enum
      |> List.cons ["rank"; "n_taxids"; "n_nonconvex"; "max_bad"; "tot_bad"]
      |> self#write_ll_tab

end
