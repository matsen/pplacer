open Ppatteries
open Subcommand
open Guppy_cmdobjs

module I = Mass_map.Indiv

let update_score ~gt ~p_exp indiv_map leaf map =
  let score =
    if not (IntMap.mem leaf indiv_map) then 0.0 else
      let indiv = IntMap.find leaf indiv_map in
      let squashed_indiv = IntMap.singleton
        leaf
        [{I.distal_bl = 0.0; I.mass = I.total_mass indiv}]
      in
      Kr_distance.dist gt p_exp indiv squashed_indiv
  in
  IntMap.add leaf score map

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit kr_cmd () as super_kr
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val verbose = flag "-v"
    (Plain (false, "If specified, write progress output to stderr."))
  val trimmed_tree_file = flag "-t"
    (Needs_argument ("trimmed tree file", "If specified, the path to write the trimmed tree to."))
  val dist_cutoff = flag "--distance"
    (Needs_argument ("", "If set to x, stop when the minimum KR distance from a voronoi region to ."))
  val leaf_cutoff = flag "--leaves"
    (Needs_argument ("", "If provided, the maximum number of leaves to cut from the tree."))
  val leaf_mass = flag "--leaf-mass"
    (Formatted (0.0, "Fraction of mass to be distributed uniformly across leaves. Default %g."))

  method specl =
    super_mass#specl
    @ super_kr#specl
    @ super_refpkg#specl
    @ super_output#specl
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
      let transform, weighting, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
      and leaf_mass_fract = fv leaf_mass
      and ch = self#out_channel |> csv_out_channel |> Csv.to_out_obj
      and verbose = fv verbose in
      let taxtree = match self#get_rpo with
        | Some rp -> Refpkg.get_tax_ref_tree rp
        | None -> Decor_gtree.of_newick_gtree gt
      and update_score = update_score ~gt ~p_exp:(fv p_exp) in
      if 0. > leaf_mass_fract || leaf_mass_fract > 1. then
        failwith ("Leaf mass fraction not between 0 and 1.");
      (* First get the mass that is not at the leaves. *)
      let mass =
        if leaf_mass_fract = 1. then IntMap.empty
        else
          Mass_map.Indiv.scale_mass
            (1. -. leaf_mass_fract)
            (Mass_map.Indiv.of_placerun transform weighting criterion pr)
      and diagram = Voronoi.of_gtree gt in
      let n_leaves = IntSet.cardinal diagram.Voronoi.all_leaves in
      let criteria =
        (match fvo dist_cutoff with
          | Some cutoff -> [fun (dist, _) -> dist > cutoff]
          | None -> [])
        @ (match fvo leaf_cutoff with
          | Some count ->
            [fun (_, v) ->
              (n_leaves - (IntSet.cardinal v.Voronoi.all_leaves)) >= count]
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
      let rec aux diagram score_map updated_leaves =
        let indiv_map = Voronoi.partition_indiv_on_leaves diagram mass in
        let score_map' = IntSet.fold
          (update_score indiv_map)
          updated_leaves
          score_map
        in
        (* Find the leaf with the least mass in its Voronoi region. When there
         * are > 1 leaves with zero mass in their regions, we get all of them. *)
        let leaf, dist = IntMap.enum score_map' |> Enum.arg_min snd in
        if List.exists ((|>) (dist, diagram)) criteria then
          diagram
        else begin
          Csv.output_record
            ch
            [Gtree.get_name taxtree leaf; Printf.sprintf "%1.6f" dist];
          if verbose then begin
            Printf.fprintf stderr "uncoloring %d (dist %1.6f)" leaf dist;
            prerr_newline ();
          end;
          let diagram', updated_leaves' = Voronoi.uncolor_leaf diagram leaf in
          aux
            diagram'
            (IntMap.remove leaf score_map')
            (IntSet.remove leaf updated_leaves')
        end
      in
      let diagram' = aux diagram IntMap.empty diagram.Voronoi.all_leaves in
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
