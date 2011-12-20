open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

let prune_notax sizemim st =
  let open Stree in
  let rec should_prune i =
    let sizem = IntMap.find i sizemim in
    ColorMap.cardinal sizem = 1 && ColorMap.mem Tax_id.NoTax sizem
  and aux = function
    | Leaf _ as l -> l
    | Node (i, subtrees) ->
      List.filter_map
        (fun t ->
          let j = top_id t in
          if should_prune j then None else Some (aux t))
        subtrees
      |> node i
  in
  aux st

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit tabular_cmd () as super_tabular

  val processes = flag "-j"
    (Formatted (2, "The number of processes to run pplacer with. default: %d"))

  method specl =
    super_refpkg#specl
  @ super_tabular#specl
  @ [
    int_flag processes;
  ]

  method desc = "infer classifications of unclassified sequences in a reference package"
  method usage = "usage: infer [options] -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp
    and td = Refpkg.get_taxonomy rp
    and seqinfo = Refpkg.get_seqinfom rp in
    let colors = Newick_gtree.leaf_label_map gt
      |> IntMap.map (Tax_seqinfo.tax_id_by_node_label seqinfo)
    and st = Gtree.get_stree gt in
    let sizemim, _ = Convex.build_sizemim_and_cutsetim (colors, st) in
    let st' = prune_notax sizemim st in
    let gt' = Gtree.set_stree gt st' in
    let prefs = Prefs.defaults () in
    prefs.Prefs.refpkg_path := fv refpkg_path;
    prefs.Prefs.children := fv processes;
    let results = RefList.empty () in
    let placerun_cb pr =
      Placerun.get_pqueries pr
      |> List.iter
          (fun pq ->
            let classif = Pquery.place_list pq
              |> List.map Placement.classif
              |> Tax_taxonomy.list_mrca td
            in
            let classif_l =
              [Tax_id.to_string classif; Tax_taxonomy.get_tax_name td classif]
            in
            List.iter
              (flip List.cons classif_l |- RefList.push results)
              (Pquery.namel pq))
    in
    File.with_temporary_out (fun ch tree_file ->
      Newick_gtree.write ch gt';
      prefs.Prefs.tree_fname := tree_file;
      dprintf "%s\n" tree_file;
      Pplacer_run.run_file
        ~placerun_cb
        prefs
        (Refpkg.get_item_path rp "aln_fasta"));
    RefList.to_list results |> self#write_ll_tab

end
