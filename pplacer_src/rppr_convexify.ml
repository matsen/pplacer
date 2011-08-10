open Ppatteries
open Subcommand
open Guppy_cmdobjs

open Convex
open Stree

let leafset tree =
  let rec aux accum = function
    | Leaf i :: rest -> aux (IntSet.add i accum) rest
    | Node (_, subtrees) :: rest -> aux accum (List.rev_append subtrees rest)
    | [] -> accum
  in
  aux IntSet.empty [tree]

type data = {
  stree: stree;
  rank: int;
  rankname: string;
  taxmap: Tax_id.tax_id IntMap.t;
  rank_tax_map: Tax_id.tax_id IntMap.t IntMap.t;
  colormap: color IntMap.t;
  not_cut: IntSet.t;
  cut_leaves: IntSet.t;
  rank_cutseqs: IntSet.t IntMap.t;
  time_delta: float;
  max_badness: int;
}

let build_reducer f argopt lst = match argopt with
  | Some arg ->
    let init, foldf, finalize = f arg in
    finalize (List.fold_left foldf init lst)
  | None -> ()

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  val discord_file = flag "-t"
    (Needs_argument ("", "If specified, the path to write the discordance tree to."))
  val cut_seqs_file = flag "--cut-seqs"
    (Needs_argument ("", "If specified, the path to write a CSV file of cut sequences per-rank to."))
  val alternates_file = flag "--alternates"
    (Needs_argument ("", "If specified, the path to write a CSV file of alternate colors per-sequence to."))
  val check_all_ranks = flag "--check-all-ranks"
    (Plain (false, "When determining alternate colors, check all ranks instead of the least recent uncut rank."))
  val badness_cutoff = flag "--cutoff"
    (Formatted (12, "Any trees with a maximum badness over this value are skipped. Default: %d."))
  val use_naive = flag "--naive"
    (Plain (false, "Use the naive convexify algorithm."))
  val timing = flag "--timing"
    (Needs_argument ("", "If specified, save timing information for solved trees to a CSV file."))
  val no_early = flag "--no-early"
    (Plain (false, "Don't terminate early when convexifying."))
  val limit_ranks = flag "--limit-rank"
    (Plain ([], "If specified, only convexify at the given ranks. Ranks are given as a comma-delimited list of names."))
  val input_tree = flag "--tree"
    (Needs_argument ("input tree", "A tree file in newick format to work on in place of a reference package."))
  val input_colors = flag "--colors"
    (Needs_argument ("input colors", "A csv file of the colors on the tree corresponding to --tree."))

  method specl = [
    string_flag discord_file;
    string_flag cut_seqs_file;
    string_flag alternates_file;
    toggle_flag check_all_ranks;
    int_flag badness_cutoff;
    toggle_flag use_naive;
    string_flag timing;
    toggle_flag no_early;
    string_list_flag limit_ranks;
    string_flag input_tree;
    string_flag input_colors;
  ] @ super_refpkg#specl

  method desc = "make the phylogeny in a reference package convex"
  method usage = "usage: convexify [-c my.refpkg | --tree my.tre --colors my.csv]"

  method private discordance_tree fname =
    let rp = self#get_rp in
    let taxtree = Refpkg.get_tax_ref_tree rp in
    let foldf discord data =
      let gt' = Decor_gtree.color_clades_above data.cut_leaves taxtree in
      (Some data.rankname, gt') :: discord
    and finalize = Phyloxml.named_gtrees_to_file fname in
    [], foldf, finalize

  method private cut_sequences fname =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp
    and gt = Refpkg.get_ref_tree rp in
    let foldf cut_seqs data =
      let taxcounts = IntMap.fold
        (fun _ ti accum ->
          Tax_id.TaxIdMap.add
            ti
            ((Tax_id.TaxIdMap.get ti 0 accum) + 1)
            accum)
        data.taxmap
        Tax_id.TaxIdMap.empty
      in
      IntSet.fold
        (fun i accum ->
          let seqname = Gtree.get_name gt i in
          let ti = IntMap.find i data.taxmap in
          [data.rankname;
           seqname;
           Tax_id.to_string ti;
           Tax_taxonomy.get_tax_name td ti;
           string_of_int (Tax_id.TaxIdMap.find ti taxcounts)]
          :: accum)
        data.cut_leaves
        cut_seqs
    and finalize = Csv.save fname in
    [], foldf, finalize

  method private alternate_colors fname =
    let rp = self#get_rp
    and check_all_ranks = fv check_all_ranks in
    let td = Refpkg.get_taxonomy rp
    and gt = Refpkg.get_ref_tree rp in
    let foldf alternates data =
      let colormap' = IntMap.filteri
        (fun k _ -> IntSet.mem k data.not_cut)
        data.colormap
      in
      let rank_alternates = alternate_colors (colormap', data.stree) in
      let rev_colormap = IntMap.fold
        (fun _ ti -> StringMap.add (Tax_taxonomy.get_tax_name td ti) ti)
        data.taxmap
        StringMap.empty
      in
      IntSet.fold
        (fun i accum ->
          let seqname = Gtree.get_name gt i in
          ColorSet.fold
            (fun candidate accum ->
              let c_ti = StringMap.find candidate rev_colormap in
              let lineage = Tax_taxonomy.get_lineage td c_ti in
              let rec aux = function
                | [] -> true
                | [ancestor] when ancestor = c_ti -> true
                | ancestor :: rest ->
                  let ancestor_rank = Tax_taxonomy.get_tax_rank td ancestor in
                  if IntMap.mem ancestor_rank data.rank_cutseqs
                    && IntSet.mem i (IntMap.find ancestor_rank data.rank_cutseqs)
                  then
                    aux rest
                  else
                    match begin
                      try
                        let orig_ancestor = IntMap.find
                          i
                          (IntMap.find ancestor_rank data.rank_tax_map)
                        in
                        ancestor = orig_ancestor
                      with
                        | Not_found -> false
                    end with
                      | true when check_all_ranks -> aux rest
                      | x -> x
              in
              if aux (List.rev lineage) then
                [data.rankname; seqname; candidate] :: accum
              else
                accum)
            (IntMap.find i rank_alternates)
            accum)
        data.cut_leaves
        alternates
    and finalize = Csv.save fname in
    [], foldf, finalize

  method private timing fname =
    let foldf timing data =
      [data.rankname;
       string_of_int data.max_badness;
       Printf.sprintf "%0.6f" data.time_delta]
      :: timing
    and finalize = Csv.save fname in
    [], foldf, finalize

  method private rp_action rp =
    let gt = Refpkg.get_ref_tree rp in
    let st = gt.Gtree.stree
    and td = Refpkg.get_taxonomy rp in
    let limit_ranks = match fv limit_ranks with
      | [] -> None
      | ranks -> Some
        (List.enum ranks
         |> Enum.map (flip String.nsplit ",")
         |> Enum.map List.enum
         |> Enum.flatten
         |> Enum.map (fun rk -> Array.findi ((=) rk) td.Tax_taxonomy.rank_names)
         |> IntSet.of_enum)
    and cutoff = fv badness_cutoff in
    let leaves = leafset st in
    Printf.printf "refpkg tree has %d leaves\n" (IntSet.cardinal leaves);
    let rank_tax_map = rank_tax_map_of_refpkg rp in
    let nu_f = if fv no_early then None else Some apart_nu in
    let _, results = Enum.fold
      (fun ((rank_cutseqs, data_list) as accum) (rank, taxmap) ->
        let colormap = IntMap.map (Tax_taxonomy.get_tax_name td) taxmap in
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
          let not_cut, omega, time_delta =
            if fv use_naive then
              let start = Sys.time () in
              let not_cut = Naive.solve (colormap, st) in
              let delta = (Sys.time ()) -. start in
              not_cut, IntSet.cardinal not_cut, delta
            else
              let start = Sys.time () in
              let phi, omega = solve ?nu_f (colormap, st) in
              let delta = (Sys.time ()) -. start in
              nodeset_of_phi_and_tree phi st, omega, delta
          in
          Printf.printf "  solved omega: %d\n" omega;
          let cut_leaves = IntSet.diff leaves not_cut in
          let rank_cutseqs' = IntMap.add
            rank
            cut_leaves
            rank_cutseqs
          in
          let data = {
            stree = st; rank = rank; rankname = rankname; taxmap = taxmap;
            colormap = colormap; cut_leaves = cut_leaves; not_cut = not_cut;
            rank_cutseqs = rank_cutseqs'; rank_tax_map = rank_tax_map;
            time_delta = time_delta; max_badness = max_bad;
          }
          in
          rank_cutseqs', data :: data_list
        end)
      (IntMap.empty, [])
      (IntMap.enum rank_tax_map |> match limit_ranks with
        | None -> identity
        | Some ranks ->
          fst
          |- flip IntSet.mem ranks
          |> Enum.filter)
    in
    let reducers =
      [
        build_reducer self#discordance_tree (fvo discord_file);
        build_reducer self#cut_sequences (fvo cut_seqs_file);
        build_reducer self#alternate_colors (fvo alternates_file);
        build_reducer self#timing (fvo timing);
      ]
    in
    List.iter ((|>) results) reducers

  method private csv_action =
    let gt = fv input_tree |> Newick_gtree.of_file in
    let namemap = gt.Gtree.bark_map
      |> IntMap.filter_map
          (fun _ b -> try Some b#get_name with Newick_bark.No_name -> None)
      |> IntMap.enum
      |> Enum.map swap
      |> StringMap.of_enum
    in
    let colormap = fv input_colors
      |> Csv.load |> List.enum
      |> Enum.map
          (function
            | [a; b] -> StringMap.find a namemap, b
            | _ -> failwith "malformed colors csv file")
      |> IntMap.of_enum
    and st = gt.Gtree.stree
    and nu_f = if fv no_early then None else Some apart_nu in
    let leaves = leafset st in
    let _, cutsetim = build_sizemim_and_cutsetim (colormap, st) in
    let cutsetim = IntMap.add (top_id st) ColorSet.empty cutsetim in
    let max_bad, tot_bad = badness cutsetim in
    if max_bad = 0 then
      print_endline "skipped: already convex"
    else if max_bad > fv badness_cutoff then
      Printf.printf "skipped: badness of %d above cutoff threshold" max_bad
    else begin
      Printf.printf "badness: %d max; %d tot" max_bad tot_bad;
      print_newline ();
      let not_cut, omega =
        if fv use_naive then
          let not_cut = Naive.solve (colormap, st) in
          not_cut, IntSet.cardinal not_cut
        else
          let phi, omega = solve ?nu_f (colormap, st) in
          nodeset_of_phi_and_tree phi st, omega
      in
      Printf.printf "solved omega: %d\n" omega;
      let cut_leaves = IntSet.diff leaves not_cut in
      begin match fvo cut_seqs_file with
        | Some fname ->
          cut_leaves
            |> IntSet.enum
            |> Enum.map (Gtree.get_name gt |- flip List.cons [])
            |> List.of_enum
            |> Csv.save fname
        | None -> ()
      end;
      begin match fvo discord_file with
        | Some fname ->
          Decor_gtree.of_newick_gtree gt
            |> Decor_gtree.color_clades_above cut_leaves
            |> Phyloxml.gtree_to_file fname
        | None -> ()
      end;

    end

  method action _ =
    match self#get_rpo with
      | Some rp -> self#rp_action rp
      | None -> self#csv_action

end
