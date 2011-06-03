open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let flip f x y = f y x

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val trimmed_tree_file = flag "-t"
    (Needs_argument ("trimmed tree file", "If specified, the path to write the trimmed tree to."))
  val leaf_mass = flag "-m"
    (Plain (0.0, "The fraction of mass to be distributed uniformly across leaves."))
  val mass_cutoff = flag "--mass"
    (Needs_argument ("mass cutoff", "If provided, the maximum value of minimum leaf mass."))
  val leaf_cutoff = flag "--leaves"
    (Needs_argument ("leaf cutoff", "If provided, the maximum number of leaves to cut from the tree."))
  val verbose = flag "-v"
    (Plain (false, "If specified, write progress output to stderr."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_output#specl
    @ [
      string_flag trimmed_tree_file;
      float_flag leaf_mass;
      float_flag mass_cutoff;
      int_flag leaf_cutoff;
      toggle_flag verbose;
    ]

  method desc = "apply voronoi"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let transform, weighting, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
      and leaf_mass_fract = fv leaf_mass
      and ch = self#out_channel
      and verbose = fv verbose in
      let taxtree = match self#get_rpo with
        | Some rp -> Refpkg.get_tax_ref_tree rp
        | None -> Decor_gtree.of_newick_gtree gt
      in
      if 0. > leaf_mass_fract || leaf_mass_fract > 1. then
        failwith ("Leaf mass fraction not between 0 and 1.");
      (* First get the mass that is not at the leaves. *)
      let mass =
        if leaf_mass_fract = 1. then IntMap.empty
        else
          Mass_map.Indiv.scale_mass
            (1. -. leaf_mass_fract)
            (Mass_map.Indiv.of_placerun transform weighting criterion pr)
      and graph = Voronoi.of_gtree gt in
      let n_leaves = IntSet.cardinal graph.Voronoi.all_leaves in
      let criteria =
        (match fvo mass_cutoff with
          | Some cutoff -> [fun (mass, _) -> mass >= cutoff]
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
              (0.0, leaf_mass_fract /. (float_of_int n_leaves)))
            graph.Voronoi.all_leaves
            mass
      in
      let sum = List.fold_left (+.) 0.0 in
      let rec aux graph =
        let mass_dist = Voronoi.distribute_mass graph mass in
        let sum_leaf leaf = sum (IntMap.get leaf [] mass_dist) in
        match IntSet.fold
          (fun leaf ->
            let mass = sum_leaf leaf in function
              | None -> Some (IntSet.singleton leaf, mass)
              | Some (_, prev_mass) when mass < prev_mass ->
                Some (IntSet.singleton leaf, mass)
              | Some (leafs, prev_mass) when mass = prev_mass && mass = 0.0 ->
                Some (IntSet.add leaf leafs, prev_mass)
              | (Some _) as prev -> prev)
          graph.Voronoi.all_leaves
          None
        with
          | None -> failwith "no leaves?"
          | Some (leafs, mass) ->
            if List.exists (fun f -> f (mass, graph)) criteria then
              graph
            else begin
              if verbose then begin
                Printf.fprintf stderr "uncoloring %d leaves (mass %1.6f)"
                  (IntSet.cardinal leafs)
                  mass;
                prerr_newline ();
              end;
              let graph', _ = Voronoi.uncolor_leaves graph leafs in
              let cut = List.map
                (fun leaf -> [string_of_int leaf; Printf.sprintf "%1.6f" mass])
                (IntSet.elements leafs)
              in
              Csv.save_out ch cut;
              aux graph'
            end
      in
      let graph' = aux graph in
      let trimmed =
        IntSet.diff
          graph.Voronoi.all_leaves
          graph'.Voronoi.all_leaves
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
