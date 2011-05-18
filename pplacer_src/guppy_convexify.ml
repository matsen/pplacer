open Subcommand
open Guppy_cmdobjs

open Convex
open MapsSets
open Stree

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  val discord_file = flag "-d"
    (Needs_argument ("discordance file", "If specified, the path to write the discordance tree to."))

  method specl = [
    string_flag discord_file;
  ] @ super_refpkg#specl


  method desc = "check a reference package"
  method usage = "usage: check_refpkg -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = gt.Gtree.stree
    and td = Refpkg.get_taxonomy rp in
    let discordance = IntMap.fold
      (fun rank colormap accum ->
        let rankname = Tax_taxonomy.get_rank_name td rank in
        Printf.printf "solving %s\n" rankname;
        let _, cutsetim = build_sizemim_and_cutsetim (colormap, st) in
        let cutsetim = IntMap.add (top_id st) ColorSet.empty cutsetim in
        let max_bad, tot_bad = badness cutsetim in
        if max_bad = 0 then begin
          Printf.printf "  already convex; skipping\n";
          accum
        end else begin
          Printf.printf "  badness: %d max; %d tot\n" max_bad tot_bad;
          let phi, nu = solve (colormap, st) in
          Printf.printf "  solved nu: %d\n" nu;
          let not_cut = nodeset_of_phi_and_tree phi st in
          let rec aux accum = function
            | Leaf i :: rest ->
              aux
                (if IntSet.mem i not_cut then
                    accum
                 else
                    IntMap.add i [Decor.red] accum)
                rest
            | Node (_, subtrees) :: rest ->
              aux accum (List.rev_append subtrees rest)
            | [] -> accum
          in
          let decor_map = aux (IntMap.empty) [st] in
          let gt' = Decor_gtree.add_decor_by_map
            (Decor_gtree.of_newick_gtree gt)
            decor_map
          in
          (Some rankname, gt') :: accum
        end)
      (rank_color_map_of_refpkg rp)
      []
    in
    match fvo discord_file with
      | Some path -> Phyloxml.named_gtrees_to_file path discordance
      | None -> ()

end
