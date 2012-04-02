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
  inherit voronoi_cmd () as super_voronoi

  val leaf_mass = flag "--leaf-mass"
    (Formatted (0.0, "Fraction of mass to be distributed uniformly across leaves. Default %g."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_voronoi#specl
    @ [
      float_flag leaf_mass;
    ]

  method desc = "finds a good collection of sequences to cut from ref tree"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let weighting, criterion = self#mass_opts
      and gt = Placerun.get_ref_tree pr |> Newick_gtree.add_zero_root_bl
      and leaf_mass_fract = fv leaf_mass
      and _, decor_tree = self#get_rpo_and_tree pr in
      if 0. > leaf_mass_fract || leaf_mass_fract > 1. then
        failwith ("Leaf mass fraction not between 0 and 1.");
      (* First get the mass that is not at the leaves. *)
      let mass =
        if leaf_mass_fract = 1. then IntMap.empty
        else
          Mass_map.Indiv.scale_mass
            (1. -. leaf_mass_fract)
            (Mass_map.Indiv.of_placerun weighting criterion pr)
      in
      let mass_cb diagram =
        if leaf_mass_fract = 0. then mass
        else
          let n_leaves = IntSet.cardinal diagram.Voronoi.all_leaves in
          IntSet.fold
            (flip
              IntMap.add_listly
              {I.distal_bl = 0.0;
               I.mass = leaf_mass_fract /. (float_of_int n_leaves)})
            diagram.Voronoi.all_leaves
            mass
      in
      self#perform_voronoi ~decor_tree gt mass_cb

    | l ->
      List.length l
      |> Printf.sprintf "voronoi takes exactly one placefile (%d given)"
      |> failwith

end
