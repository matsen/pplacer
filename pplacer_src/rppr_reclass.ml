open Subcommand
open Guppy_cmdobjs
open Ppatteries
open Convex

let format_float f =
  match classify_float f with
    | FP_infinite
    | FP_nan -> "-"
    | _ -> Printf.sprintf "%g" f

let contained_in inner outer =
  ColorMap.for_all
    (fun k v -> ColorMap.get k 0 outer = v)
    inner

let uninformative_nodes top_sizem sizemim st =
  let prelim = IntMap.fold
    (fun i sizem accum ->
      if ColorMap.cardinal sizem = 2 && contained_in sizem top_sizem then
        IntSet.add i accum
      else accum)
    sizemim
    IntSet.empty
  in
  let open Stree in
  let rec aux accum = function
    | [] -> accum
    | Node (i, subtrees) :: rest ->
      let accum' =
        if IntSet.mem i accum then
          List.fold_left (top_id |- IntSet.add |> flip) accum subtrees
        else accum
      and rest' = List.append rest subtrees in
      aux accum' rest'
    | Leaf _ :: rest -> aux accum rest
  in
  aux prelim [st]

class cmd () =
object (self)
  inherit Rppr_infer.cmd () as super_infer

  val suggestion_tree = flag "-t"
    (Needs_argument ("", "If specified, the path to write the suggestion tree to."))

  method specl = super_infer#specl @ [string_flag suggestion_tree]

  method desc = "reclassifies nonconvex sequences in a reference package"
  method usage = "usage: reclass [options] -c my.refpkg"

  method action _ =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = Gtree.get_stree gt
    and leaf_labels = Newick_gtree.leaf_label_map gt
    and td = Refpkg.get_taxonomy rp
    and seqinfo = Refpkg.get_seqinfom rp in
    let taxa_set colorm needle =
      IntMap.fold
        (fun i ti accum -> if ti = needle then IntSet.add i accum else accum)
        colorm
        IntSet.empty
    and pair_dist = Edge_rdist.build_pairwise_dist gt
      |> Edge_rdist.find_pairwise_dist
    and seq_nodes = IntMap.enum leaf_labels
      |> Enum.map swap
      |> StringMap.of_enum
    in
    let med_cv_taxdist_from colorm ti j =
      let ts = taxa_set colorm ti in
      if IntSet.is_empty ts then nan, nan else (* ... *)
      let distances = IntSet.elements ts
        |> List.map (fun i -> pair_dist i 0. j 0.)
      and fcardinal = IntSet.cardinal ts |> float_of_int in
      let mean = List.fsum distances /. fcardinal in
      median distances,
      List.fold_left (fun a x -> (x -. mean) ** 2. +. a) 0. distances
        |> flip (/.) (fcardinal -. 1.)
        |> sqrt
        |> flip (/.) mean
    and rank_tax_map = rank_tax_map_of_refpkg rp in
    let rank, colors = Enum.find
      (fun (_, colors) ->
        let _, cutsetim = build_sizemim_and_cutsetim (colors, st) in
        let max_bad, _ = badness cutsetim in
        max_bad > 0)
      (IntMap.backwards rank_tax_map)
    in
    Tax_taxonomy.get_rank_name td rank
      |> dprintf "reclassifying at %s\n";
    let phi, _ = solve (colors, st) in
    let not_cut = nodeset_of_phi_and_tree phi st in
    let cut = Stree.leaf_ids st
      |> IntSet.of_list
      |> flip IntSet.diff not_cut
    in
    let notax_colors = IntSet.fold (flip IntMap.add Tax_id.NoTax) cut colors in
    let uncolored_colors = IntMap.filter ((<>) Tax_id.NoTax) notax_colors in
    let notax_sizemim, _ = build_sizemim_and_cutsetim (notax_colors, st) in
    let st' = Rppr_infer.prune_notax notax_sizemim st in
    let gt' = Gtree.set_stree gt st' in
    let results = self#place_on_rp rp gt' in
    dprint "finished classifying\n";
    let alternate_map = alternate_colors (uncolored_colors, st)
    and orig_sizemim, _ = build_sizemim_and_cutsetim (colors, st) in
    let orig_sizem = IntMap.find (Stree.top_id st) orig_sizemim
    and notax_sizem = IntMap.find (Stree.top_id st) notax_sizemim
    and taxtree = Refpkg.get_tax_ref_tree rp in
    let uninformative = uninformative_nodes orig_sizem orig_sizemim st in
    let bm, rows = List.fold_left
      (fun (bm, rows) (ti, _, seq) ->
        let prev_ti = Tax_seqinfo.tax_id_by_node_label seqinfo seq
        and i = StringMap.find seq seq_nodes in
        let alternates = IntMap.find i alternate_map
          |> ColorSet.elements
          |> List.map (Tax_taxonomy.get_lineage td)
          |> List.flatten
          |> ColorSet.of_list
        and prev_count = ColorMap.get prev_ti 0 orig_sizem
        and new_name = Tax_taxonomy.get_tax_name td ti
        and prev_med, prev_cv = med_cv_taxdist_from uncolored_colors prev_ti i
        and new_med, new_cv = med_cv_taxdist_from uncolored_colors ti i in
        IntMap.modify
          i
          (fun b -> Printf.sprintf "%s -> %s" seq new_name |> b#set_node_label)
          bm,
        [seq;
         Tax_taxonomy.get_tax_name td prev_ti;
         Tax_id.to_string prev_ti;
         new_name;
         Tax_id.to_string ti;
         ColorSet.mem ti alternates |> string_of_bool;
         IntSet.mem i uninformative |> string_of_bool;
         prev_med |> format_float;
         prev_cv *. 100. |> format_float;
         new_med |> format_float;
         new_cv *. 100. |> format_float;
         prev_count |> string_of_int;
         prev_count - ColorMap.get prev_ti 0 notax_sizem |> string_of_int;
        ] :: rows)
      (Gtree.get_bark_map taxtree, [])
      results
    in
    rows
    |> List.cons
        ["seq_name"; "old_name"; "old_taxid"; "new_name"; "new_taxid";
         "makes_convex"; "uninformative"; "old_median_dist"; "old_avg_cv";
         "new_median_dist"; "new_avg_cv"; "n_with_old"; "n_nonconvex"]
    |> self#write_ll_tab;
    match fvo suggestion_tree with
      | None -> ()
      | Some fname ->
        Gtree.set_bark_map taxtree bm
          |> Decor_gtree.color_clades_above cut
          |> Decor_gtree.color_clades_above ~color:Decor.yellow uninformative
          |> Decor_gtree.consolidate_colors Decor_gtree.list_color_average
          |> Phyloxml.gtree_to_file fname

end
