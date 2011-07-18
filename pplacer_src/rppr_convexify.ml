open Subcommand
open Guppy_cmdobjs

open Convex
open MapsSets
open Stree

let flip f x y = f y x
let apply f x = f x
let uncurry f (a, b) = f a b

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
    (Plain (false, "Show timing information for solved trees."))

  method specl = [
    string_flag discord_file;
    string_flag cut_seqs_file;
    string_flag alternates_file;
    toggle_flag check_all_ranks;
    int_flag badness_cutoff;
    toggle_flag use_naive;
    toggle_flag timing;
  ] @ super_refpkg#specl


  method desc = "check a reference package"
  method usage = "usage: check_refpkg -c my.refpkg"

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
      let colormap' = IntMap.filter
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

  method private timing do_timing =
    let foldf accum data =
      IntMap.add data.rank data accum
    and finalize = if not do_timing then fun _ -> () else fun timing ->
      print_endline "\ntiming information:";
      IntMap.iter
        (fun _ data ->
          Printf.printf "%s (%d): %0.4fs\n"
            data.rankname
            data.max_badness
            data.time_delta)
        timing
    in
    IntMap.empty, foldf, finalize

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = gt.Gtree.stree
    and td = Refpkg.get_taxonomy rp
    and cutoff = fv badness_cutoff in
    let leaves = leafset st in
    Printf.printf "refpkg tree has %d leaves\n" (IntSet.cardinal leaves);
    let rank_tax_map = rank_tax_map_of_refpkg rp in
    let _, results = IntMap.fold
      (fun rank taxmap ((rank_cutseqs, data_list) as accum) ->
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
              let phi, omega = solve (colormap, st) in
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
      rank_tax_map
      (IntMap.empty, [])
    in
    let reducers =
      [
        build_reducer self#discordance_tree (fvo discord_file);
        build_reducer self#cut_sequences (fvo cut_seqs_file);
        build_reducer self#alternate_colors (fvo alternates_file);
        build_reducer self#timing (fvo timing);
      ]
    in
    List.iter ((flip apply) results) reducers

end
