open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit output_cmd () as super_output

  method specl = super_refpkg#specl @ super_output#specl

  method desc = "check a reference package"
  method usage = "usage: check_refpkg -c my.refpkg"

  method action _ =
    let ch = self#out_channel |> csv_out_channel |> Csv.to_out_obj
    and rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp
    and td = Refpkg.get_taxonomy rp in
    let st = gt.Gtree.stree in
    Csv.output_record
      ch
      ["rank"; "n_taxids"; "n_nonconvex"; "max_bad"; "tot_bad"];
    IntMap.iter
      (fun rank taxmap ->
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
        Csv.output_all
          ch
          [(Tax_taxonomy.get_rank_name td rank) ::
              (List.map
                 string_of_int
                 [ColorSet.cardinal all_colors;
                  ColorSet.cardinal unconvex_colors;
                  max_bad;
                  tot_bad])]
      )
      (rank_tax_map_of_refpkg rp)

end
