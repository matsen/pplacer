open Ppatteries
open Subcommand
open Guppy_cmdobjs

module I = Mass_map.Indiv

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd ~default_to_csv:true () as super_tabular

  val verbose = flag "-v"
    (Plain (false, "If specified, write progress output to stderr."))
  val trimmed_tree_file = flag "-t"
    (Needs_argument ("trimmed tree file", "If specified, the path to write the trimmed tree to."))
  val leaf_cutoff = flag "--leaves"
    (Needs_argument ("", "The maximum number of leaves to keep in the tree."))
  val leaf_mass = flag "--leaf-mass"
    (Formatted (0.0, "Fraction of mass to be distributed uniformly across leaves. Default %g."))
  val algorithm = flag "--algorithm"
    (Formatted ("full",
                "Which algorithm to use to prune leaves. Choices are 'greedy', 'full', and 'force'. Default %s."))
  val all_eclds_file = flag "--all-eclds-file"
    (Needs_argument ("", "If specified, write out a csv file containing every intermediate computed ECLD."))
  val soln_log = flag "--log"
    (Needs_argument ("", "If specified with the full algorithm, write out a csv file containing solutions at \
                          every internal node."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_tabular#specl
    @ [
      toggle_flag verbose;
      string_flag trimmed_tree_file;
      int_flag leaf_cutoff;
      float_flag leaf_mass;
      string_flag algorithm;
      string_flag all_eclds_file;
      string_flag soln_log;
    ]

  method desc = "apply voronoi"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let alg = match fv algorithm with
        | "greedy" -> (module Voronoi.Greedy: Voronoi.Alg)
        | "full" -> (module Voronoi.Full: Voronoi.Alg)
        | "force" -> (module Voronoi.Forced: Voronoi.Alg)
        | x -> failwith (Printf.sprintf "unknown algorithm: %s" x)
      and weighting, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr
      and leaf_mass_fract = fv leaf_mass
      and verbose = fv verbose
      and leaf_cutoff = fv leaf_cutoff in
      Voronoi.Full.csv_log :=
        fvo soln_log
          |> Option.map (open_out |- csv_out_channel |- Csv.to_out_obj);
      let module Alg = (val alg: Voronoi.Alg) in
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
            (Mass_map.Indiv.of_placerun weighting criterion pr)
      and diagram = Voronoi.of_gtree gt in
      let n_leaves = IntSet.cardinal diagram.Voronoi.all_leaves in
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
      let solm = Alg.solve
        ~strict:(fvo all_eclds_file |> Option.is_none)
        ~verbose
        gt
        mass
        leaf_cutoff
      in
      let {Voronoi.leaves} = IntMap.find leaf_cutoff solm in
      let cut_leaves = gt
        |> Gtree.leaf_ids
        |> IntSet.of_list
        |> flip IntSet.diff leaves
      in

      begin match fvo trimmed_tree_file with
        | Some fname ->
          Decor_gtree.color_clades_above cut_leaves taxtree
            |> Phyloxml.gtree_to_file fname
        | None -> ()
      end;

      begin match fvo all_eclds_file with
        | Some fname ->
          IntMap.enum solm
            |> Enum.map
                (fun (c, {Voronoi.work}) ->
                  [string_of_int c; Printf.sprintf "%g" work])
            |> List.of_enum
            |> Csv.save fname
        | None -> ()
      end;

      cut_leaves
        |> IntSet.enum
        |> Enum.map (Gtree.get_name gt |- flip List.cons [])
        |> List.of_enum
        |> self#write_ll_tab;

    | _ -> failwith "voronoi takes exactly one placefile"

end
