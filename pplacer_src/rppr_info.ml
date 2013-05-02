open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit tabular_cmd () as super_tabular

  val taxonomic = flag "--taxonomic"
    (Plain (false, "Show by-rank taxonomic information"))

  method specl = super_refpkg#specl
                 @ super_tabular#specl @
                 [
                   toggle_flag taxonomic;
                 ]

  method desc = "gives information about a reference package"
  method usage = "usage: info -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp
    and td = Refpkg.get_taxonomy rp in
    let st = gt.Gtree.stree in
    let top_id = Stree.top_id st in
    if fv taxonomic then begin
      rank_tax_map_of_refpkg rp
        |> IntMap.enum
        |> Enum.map
            (fun (rank, taxmap) ->
              let sizemim, cutsetim = build_sizemim_and_cutsetim (taxmap, st) in
              let cutsetim = IntMap.add top_id ColorSet.empty cutsetim in
              let unconvex_colors = IntMap.fold
                (fun _ colors unconvex ->
                  if ColorSet.cardinal colors < 2 then unconvex else
                    ColorSet.union unconvex colors)
                cutsetim
                ColorSet.empty
              and max_bad, tot_bad = badness cutsetim in
              (Tax_taxonomy.get_rank_name td rank) ::
                (List.map
                   string_of_int
                   [ColorMap.cardinal (IntMap.find top_id sizemim);
                    ColorSet.cardinal unconvex_colors;
                    max_bad;
                    tot_bad]))
        |> List.of_enum
        |> List.cons ["rank"; "n_taxids"; "n_nonconvex"; "max_bad"; "tot_bad"]
        |> self#write_ll_tab
    end
    else
      Printf.printf "%s: %d leaves, %d taxids\n"
        (Refpkg.get_name rp)
        (Stree.n_taxa st)
        (Tax_id.TaxIdMap.cardinal td.Tax_taxonomy.tax_name_map)

end
