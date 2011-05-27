open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_output#specl

  method desc = "apply voronoi"
  method usage = "usage: voronoi [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let transform, weighting, criterion = self#mass_opts in
      let mass = Mass_map.Indiv.of_placerun transform weighting criterion pr
      and graph = Voronoi.of_gtree (Placerun.get_ref_tree pr) in
      let mass_dist = Voronoi.distribute_mass graph mass in
      IntMap.iter
        (fun e fl ->
          Printf.printf "%d " e;
          List.iter (Printf.printf "%0.6f ") fl;
          print_newline ())
        mass_dist;
      let sum = List.fold_left (+.) 0.0 in
      let rec aux graph =
        let mass_dist = Voronoi.distribute_mass graph mass in
        let sum_leaf leaf = sum (IntMap.get leaf [] mass_dist) in
        match IntSet.fold
          (fun leaf ->
            let mass = sum_leaf leaf in function
              | None -> Some (leaf, mass)
              | Some (_, prev_mass) when mass < prev_mass -> Some (leaf, mass)
              | (Some _) as prev -> prev)
          graph.Voronoi.all_leaves
          None
        with
          | None -> failwith "no leaves?"
          | Some (leaf, mass) ->
            Printf.printf "smallest mass: %d (%1.6f); %d leaves remaining\n"
              leaf
              mass
              (IntSet.cardinal graph.Voronoi.all_leaves);
            let graph', updated = Voronoi.uncolor_leaf graph leaf in
            IntSet.ppr Format.std_formatter updated;
            Format.print_newline ();
            if IntSet.cardinal graph.Voronoi.all_leaves <= 2 then begin
              Printf.printf "remaining: ";
              IntSet.ppr Format.std_formatter graph.Voronoi.all_leaves;
              Format.print_newline ();
            end else
              aux graph'
      in
      aux graph


    | _ -> failwith "voronoi takes exactly one placefile"

end
