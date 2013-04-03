open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

let prune_notax_to_pql sizemim colorm gt =
  let should_prune i =
    let sizem = IntMap.find i sizemim in
    ColorMap.cardinal sizem = 1 && ColorMap.mem Tax_id.NoTax sizem
  and placement_transform i p =
    IntMap.find i colorm
      |> Placement.add_classif p
  in
  Newick_gtree.prune_to_pql ~placement_transform should_prune gt

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd ~show_fname:false ~prefix_required:true () as super_output

  (* required to get #write_placefile *)
  method private placefile_action _ = ()

  val prune_taxids = flag "-r"
    (Plain ([], "Comma-delimited list of tax_ids to prune."))

  method specl =
    super_refpkg#specl
  @ super_output#specl
  @ [delimited_list_flag prune_taxids]

  method desc = "makes a simulation by taking out taxids and turning them into fake placements"
  method usage = "usage: prepsim [options] -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let pruned = fv prune_taxids
      |> List.map Tax_id.of_string
      |> Tax_id.TaxIdSet.of_list
    and prefix = self#single_prefix ~requires_user_prefix:true ()
    and gt = Refpkg.get_ref_tree rp |> Newick_gtree.add_zero_root_bl
    and td = Refpkg.get_taxonomy rp
    and seqinfo = Refpkg.get_seqinfom rp in
    let should_keep ti =
      Tax_taxonomy.get_lineage td ti
        |> Tax_id.TaxIdSet.of_list
        |> Tax_id.TaxIdSet.disjoint pruned
    and leaf_labels = Newick_gtree.leaf_label_map gt in
    let colors =
      IntMap.map
        (Tax_seqinfo.tax_id_by_node_label seqinfo)
        leaf_labels
    and st = Gtree.get_stree gt in
    let colors_notax = IntMap.map
      (junction should_keep identity (const Tax_id.NoTax))
      colors
    in
    let sizemim, _ = build_sizemim_and_cutsetim (colors_notax, st) in
    let no_tax =
      IntMap.fold
        (fun i ti accum ->
          if ti = Tax_id.NoTax then
            StringSet.add (Gtree.get_node_label gt i) accum
          else accum)
        colors_notax
        StringSet.empty
    and gt', pql = prune_notax_to_pql sizemim colors gt in
    if StringSet.is_empty no_tax then
      dprint "warning: nothing was cut off the tree\n";
    let gt'', transm = Newick_gtree.consolidate gt' in
    Pquery.translate_pql transm pql
    |> Placerun.make gt'' "inferred"
    |> self#write_placefile (prefix ^ "inferred.jplace");
    Newick_gtree.to_file gt'' (prefix ^ "cut.tre");
    StringSet.elements no_tax
      |> List.map (flip List.cons [])
      |> Csv.save (prefix ^ "cut.csv");
    let no_tax_aln, new_ref_aln = Refpkg.get_aln_fasta rp
      |> Array.partition (fst %> flip StringSet.mem no_tax)
    in
    Alignment.to_fasta no_tax_aln (prefix ^ "cut.fasta");
    Alignment.to_fasta new_ref_aln (prefix ^ "new_ref.fasta");
    dprintf "taxit update %S %S %S\n"
      (fv refpkg_path)
      (Printf.sprintf "tree=%scut.tre" prefix)
      (Printf.sprintf "aln_fasta=%snew_ref.fasta" prefix)

end
