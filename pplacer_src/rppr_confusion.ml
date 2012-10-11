open Ppatteries
open Subcommand
open Guppy_cmdobjs

open Convex

let prune_edges_by_confidence cutoff gt =
  let should_prune i =
    (try
      Newick_gtree.get_confidence gt i
     with Newick_gtree.Lacking_confidence _ -> 1.)
    < cutoff
  in
  Newick_gtree.prune_edges should_prune gt

let mrcam_of_refpkg rp =
  let gt = Refpkg.get_ref_tree rp
  and seqinfo = Refpkg.get_seqinfom rp
  and td = Refpkg.get_taxonomy rp in
  let leaf_taxon =
    Gtree.get_node_label gt
      |- Tax_seqinfo.tax_id_by_node_label seqinfo
  in
  let open Stree in
  let rec aux accum = function
    | [] -> accum
    | Leaf i :: rest -> aux (IntMap.add i (leaf_taxon i) accum) rest
    | Node (i, subtrees) as t :: rest ->
      Stree.leaf_ids t
        |> List.map leaf_taxon
        |> Tax_taxonomy.list_mrca td
        |> flip (IntMap.add i) accum
        |> flip aux (List.append subtrees rest)
  in
  aux IntMap.empty [Gtree.get_stree gt]

let enum_pairs s =
  ColorSet.elements s
    |> flip EnumFuns.combinations 2
    |> Enum.map (List.enum |- Tuple2.of_enum)

module OrderedColorPair = struct
  type t = color * color
  let compare = Tuple2.compare ~cmp1:compare ~cmp2:compare
end

module PprColorPair = struct
  type t = color * color
  let ppr ff (a, b) =
    Format.fprintf ff "(%a, %a)" Tax_id.ppr a Tax_id.ppr b
end

module ColorPairMap = BetterMap (Map.Make(OrderedColorPair)) (PprColorPair)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit tabular_cmd ~default_to_csv:true () as super_tabular

  val pruned_file = flag "-t"
    (Needs_argument ("", "If specified, the path to write the pruned tree to."))
  val bootstrap_cutoff = flag "--bootstrap-cutoff"
    (Formatted (0.5, "Prune edges with a bootstrap below this value. Default: %g."))
  val edge_count_cutoff = flag "--edge-count-cutoff"
    (Formatted (3, "Only count edges as confusion if there at least this many edges. Default: %d."))

  method specl =
    super_refpkg#specl
  @ super_tabular#specl
  @ [
    string_flag pruned_file;
    float_flag bootstrap_cutoff;
    int_flag edge_count_cutoff;
  ]

  method desc = "identifies minimal leaf set to cut for taxonomic concordance"
  method usage = "usage: confusion -c my.refpkg"

  method private action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp
      |> prune_edges_by_confidence (fv bootstrap_cutoff)
    and td = Refpkg.get_taxonomy rp in
    let rp' = Refpkg.set_ref_tree gt rp in
    let tax_name = function
      | Tax_id.NoTax -> "-"
      | ti -> Tax_taxonomy.get_tax_name td ti
    in
    let dt = mrcam_of_refpkg rp'
      |> IntMap.map (fun ti -> [Decor.Taxinfo (ti, tax_name ti)])
      |> Decor_gtree.add_decor_by_map (Decor_gtree.of_newick_gtree gt)
    and write_trees = match fvo pruned_file with Some _ -> true | None -> false
    and trees = RefList.empty () in
    if write_trees then
      RefList.push trees (None, dt);
    let st = Gtree.get_stree gt in
    let rank_tax_map = rank_tax_map_of_refpkg rp' in
    let solve taxmap =
      let _, cutsetim = build_sizemim_and_cutsetim (taxmap, st) in
      let cutsetim = IntMap.add (Stree.top_id st) ColorSet.empty cutsetim in
      IntMap.values cutsetim
        |> Enum.map enum_pairs
        |> Enum.flatten
        |> ColorPairMap.histogram_of_enum

    in
    let min_count = fv edge_count_cutoff in
    IntMap.enum rank_tax_map
    |> Enum.map (fun (rank, taxmap) ->
      let rankname = Tax_taxonomy.get_rank_name td rank in
      solve taxmap
      |> ColorPairMap.filter ((<=) min_count)
      |> ColorPairMap.enum
      |> Enum.map
          (fun ((a, b), count) ->
            [rankname; string_of_int count;
             Tax_id.to_string a; tax_name a;
             Tax_id.to_string b; tax_name b]))
    |> Enum.flatten
    |> List.of_enum
    |> List.cons
        ["rank"; "edge_count"; "node1"; "node1_name"; "node2"; "node2_name"]
    |> self#write_ll_tab;

    begin match fvo pruned_file with
    | Some path -> RefList.to_list trees |> Phyloxml.named_gtrees_to_file path
    | None -> ()
    end;

end
