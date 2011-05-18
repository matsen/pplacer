open Subcommand
open Guppy_cmdobjs

open Convex
open MapsSets
open Stree

let leafset tree =
  let rec aux accum = function
    | Leaf i :: rest -> aux (IntSet.add i accum) rest
    | Node (_, subtrees) :: rest -> aux accum (List.rev_append subtrees rest)
    | [] -> accum
  in
  aux IntSet.empty [tree]

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  val discord_file = flag "-d"
    (Needs_argument ("discordance file", "If specified, the path to write the discordance tree to."))
  val cut_seqs_file = flag "--cut-seqs"
    (Needs_argument ("cut sequences file", "If specified, the path to write a CSV file of cut sequences per-rank to."))
  val badness_cutoff = flag "--cutoff"
    (Formatted (12, "Any trees with a maximum badness over this value are skipped. Default: %d."))

  method specl = [
    string_flag discord_file;
    string_flag cut_seqs_file;
    int_flag badness_cutoff;
  ] @ super_refpkg#specl


  method desc = "check a reference package"
  method usage = "usage: check_refpkg -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = gt.Gtree.stree
    and td = Refpkg.get_taxonomy rp
    and cutoff = fv badness_cutoff in
    let discordance, cut_sequences = IntMap.fold
      (fun rank colormap ((discord, cut_seqs) as accum) ->
        let rankname = Tax_taxonomy.get_rank_name td rank in
        Printf.printf "solving %s" rankname;
        print_newline ();
        let _, cutsetim = build_sizemim_and_cutsetim (colormap, st) in
        let cutsetim = IntMap.add (top_id st) ColorSet.empty cutsetim in
        let max_bad, tot_bad = badness cutsetim in
        if max_bad = 0 then begin
          Printf.printf "  skipping: already convex\n";
          accum
        end else if max_bad > cutoff then begin
          Printf.printf
            "  skipping: badness of %d above cutoff threshold\n"
            max_bad;
          accum
        end else begin
          Printf.printf "  badness: %d max; %d tot" max_bad tot_bad;
          print_newline ();
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
          let cut_leaves = IntSet.diff (leafset st) not_cut in
          (Some rankname, gt') :: discord,
          IntSet.fold
            (fun i accum -> [rankname; Gtree.get_name gt i] :: accum)
            cut_leaves
            cut_seqs
        end)
      (rank_color_map_of_refpkg rp)
      ([], [])
    in
    begin match fvo discord_file with
      | Some path -> Phyloxml.named_gtrees_to_file path discordance
      | None -> ()
    end;
    begin match fvo cut_seqs_file with
      | Some path -> Csv.save path cut_sequences
      | None -> ()
    end

end
