open Ppatteries
open Subcommand
open Guppy_cmdobjs

module I = Mass_map.Indiv

let update_score ~p_exp v indiv leaf map =
  let v', _ = Voronoi.uncolor_leaf v leaf in
  Voronoi.ecld ~p_exp v' (Voronoi.partition_indiv_on_leaves v' indiv)
  |> flip (IntMap.add leaf) map

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit kr_cmd () as super_kr
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd ~default_to_csv:true () as super_tabular

  val verbose = flag "-v"
    (Plain (false, "If specified, write progress output to stderr."))
  val trimmed_tree_file = flag "-t"
    (Needs_argument ("trimmed tree file", "If specified, the path to write the trimmed tree to."))
  val dist_cutoff = flag "--distance"
    (Needs_argument ("", "If set to x, stop when the minimum KR distance from a voronoi region to ."))
  val leaf_cutoff = flag "--leaves"
    (Needs_argument ("", "If provided, the maximum number of leaves to keep in the tree."))
  val leaf_mass = flag "--leaf-mass"
    (Formatted (0.0, "Fraction of mass to be distributed uniformly across leaves. Default %g."))

  method specl =
    super_mass#specl
    @ super_kr#specl
    @ super_refpkg#specl
    @ super_tabular#specl
    @ [
      toggle_flag verbose;
      string_flag trimmed_tree_file;
      float_flag dist_cutoff;
      int_flag leaf_cutoff;
      float_flag leaf_mass;
    ]

  method desc = "apply voronoi"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let weighting, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
      and leaf_mass_fract = fv leaf_mass
      and verbose = fv verbose in
      let taxtree = match self#get_rpo with
        | Some rp -> Refpkg.get_tax_ref_tree rp
        | None -> Decor_gtree.of_newick_gtree gt
      and update_score = update_score ~p_exp:(fv p_exp) in
      if 0. > leaf_mass_fract || leaf_mass_fract > 1. then
        failwith ("Leaf mass fraction not between 0 and 1.");
      (* First get the mass that is not at the leaves. *)
      let mass =
        if leaf_mass_fract = 1. then IntMap.empty
        else
          Mass_map.Indiv.scale_mass
            (1. -. leaf_mass_fract)
            (Mass_map.Indiv.of_placerun weighting criterion pr)
      and diagram = Voronoi.of_gtree gt in
      let n_leaves = IntSet.cardinal diagram.Voronoi.all_leaves in
      let criteria =
        (match fvo dist_cutoff with
          | Some cutoff -> [fun (dist, _) -> dist > cutoff]
          | None -> [])
        @ (match fvo leaf_cutoff with
          | Some count ->
            [fun (_, v) ->
              IntSet.cardinal v.Voronoi.all_leaves <= count]
          | None -> [])
      in
      if List.length criteria = 0 then
        failwith "at least one cutoff criteria must be specified";
      (* Next add the mass at the leaves. *)
      let mass =
        if leaf_mass_fract = 0. then mass
        else
          IntSet.fold
            (flip
              IntMap.add_listly
              {I.distal_bl = 0.0;
               I.mass = leaf_mass_fract /. (float_of_int n_leaves)})
            diagram.Voronoi.all_leaves
            mass
      in
      (* This is the central recursion that finds the Voronoi region with the
       * least mass and deletes its leaf from the corresponding set. We keep a
       * map of the scores for each leaf in the Voronoi region, updated at each
       * iteration with only the leaves which were touched in the last pass. *)
      let rec aux diagram cut score_map updated_leaves =
        let score_map' = IntSet.fold
          (update_score diagram mass)
          updated_leaves
          score_map
        in
        (* Find the leaf with the least mass in its Voronoi region. When there
         * are > 1 leaves with zero mass in their regions, we get all of them. *)
        let leaf, dist = IntMap.enum score_map' |> Enum.arg_min snd in
        if List.exists ((|>) (dist, diagram)) criteria then
          diagram, cut
        else begin
          if verbose then begin
            Printf.fprintf stderr "uncoloring %d (score %1.6f)" leaf dist;
            prerr_newline ();
          end;
          let diagram', updated_leaves' = Voronoi.uncolor_leaf diagram leaf in
          let cut' =
            [Voronoi.partition_indiv_on_leaves diagram' mass
             |> Voronoi.ecld diagram'
             |> Printf.sprintf "%1.10f"] :: cut
          in
          if List.exists ((|>) (dist, diagram')) criteria then
            diagram', cut'
          else
            aux
              diagram'
              cut'
              (IntMap.remove leaf score_map')
              (IntSet.remove leaf updated_leaves')
        end
      in
      let diagram', cut = aux
        diagram
        []
        IntMap.empty
        diagram.Voronoi.all_leaves
      in
      if verbose then
        Voronoi.ecld diagram' (Voronoi.partition_indiv_on_leaves diagram' mass)
          |> Printf.fprintf stderr "ECLD: %0.14g\n";
      self#write_ll_tab cut;
      let trimmed =
        IntSet.diff
          diagram.Voronoi.all_leaves
          diagram'.Voronoi.all_leaves
      in
      let decor = Decor_gtree.color_clades_above trimmed taxtree in
      begin match fvo trimmed_tree_file with
        | Some fname ->
          Phyloxml.named_gtrees_to_file
            fname
            [Some "cut leaves", decor]
        | None -> ()
      end

    | _ -> failwith "voronoi takes exactly one placefile"

end
