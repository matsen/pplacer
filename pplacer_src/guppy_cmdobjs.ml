(*
 * This is for the hierarchy of common command objects.
 *)

open Subcommand
open MapsSets


(* *** general objects *** *)

class outfile_cmd () =
object
  val out_fname = flag "-o"
    (Plain ("-", "Set the filename to write to. Otherwise write to stdout."))
  method specl = [ string_flag out_fname; ]

  method private out_channel =
    match fv out_fname with
      | "-" -> stdout
      | s -> open_out s
end

class out_prefix_cmd () =
object
  val out_prefix = flag "-o"
    (Needs_argument ("out-prefix", "Set the prefix to write to. Required."))
  method specl = [ string_flag out_prefix; ]
end

class out_dir_cmd () =
object
  val out_dir = flag "--out-dir"
    (Plain (".", "Specify the directory to write place files to."))
  method specl = [ string_flag out_dir; ]
end

class rng_cmd () =
object
  val seed = flag "--seed"
    (Formatted (1, "Set the random seed, an integer > 0. Default is %d."))
  method specl = [ int_flag seed; ]

  method private rng =
    let rng = Gsl_rng.make Gsl_rng.KNUTHRAN2002 in
    Gsl_rng.set rng (Nativeint.of_int (fv seed));
    rng
end


(* *** pplacer objects *** *)

class refpkg_cmd ~required =
object(self)
  val refpkg_path = flag "-c"
    (Needs_argument ("reference package path",
                     if required
                     then "Reference package path. Required."
                     else "Reference package path."))
  method specl = [ string_flag refpkg_path; ]

  method private get_rp = Refpkg.of_path (fv refpkg_path)
  method private get_rpo =
    match fvo refpkg_path with
      | Some path -> Some (Refpkg.of_path path)
      | None -> None

  method private check_rpo_tree name t =
    match self#get_rpo with
    | None -> ()
    | Some rp -> Refpkg.check_tree_approx rp name t

  method private check_placerunl =
    List.iter
      (fun pr ->
        self#check_rpo_tree (Placerun.get_name pr) (Placerun.get_ref_tree pr))

  method private get_rpo_and_tree pr =
    let alt_tree = Decor_gtree.of_newick_gtree pr.Placerun.ref_tree in
    match self#get_rpo with
      | None -> (None, alt_tree)
      | Some rp ->
        Refpkg.pr_check_tree_approx rp pr;
        if Refpkg.tax_equipped rp then (Some rp, Refpkg.get_tax_ref_tree rp)
        else (None, alt_tree)
end


module SM = MapsSets.StringMap

(* *** accessing placefiles *** *)
(* our strategy is to load the placefiles in to memory when we need them, but if
  * they are already in memory, then we use them *)
let placerun_map = ref SM.empty
let placerun_by_name fname =
  if SM.mem fname !placerun_map then
    SM.find fname !placerun_map
  else begin
    let pr = Placerun_io.filtered_of_file fname in
    if 0 = Placerun.n_pqueries pr then failwith (fname^" has no placements!");
    placerun_map := SM.add fname pr !placerun_map;
    pr
  end

class virtual placefile_cmd () =
object (self)
  method virtual private placefile_action: 'a Placerun.placerun list -> unit
  method action fnamel =
    let prl = List.map placerun_by_name fnamel in
    self#placefile_action prl
end


(* *** mass-related objects *** *)

class mass_cmd () =
object
  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for the weight."))
  val weighted = flag "--unweighted"
    (Plain (true, "Treat every placement as a point mass concentrated on the highest-weight placement."))
  val transform = flag "--transform"
    (Plain ("", "A transform to apply to the read multiplicities before calculating. \
    Options are 'log' and 'unit'. Default is no transform."))
  method specl = [
    toggle_flag use_pp;
    toggle_flag weighted;
    string_flag transform;
  ]

  method private mass_opts = (
    Mass_map.transform_of_str (fv transform),
    (if fv weighted then Mass_map.Weighted else Mass_map.Unweighted),
    (if fv use_pp then Placement.post_prob else Placement.ml_ratio)
  )
end

class kr_cmd () =
object
  val p_exp = flag "-p"
    (Plain (1., "The exponent for the integration, i.e. the value of p in Z_p."))
  val normalize = flag "--normalize"
    (Plain ("", "Divide KR by a given value. Legal arguments are \"tree-length\"."))
  method specl = [ float_flag p_exp; string_flag normalize; ]
end


(* *** visualization-related objects *** *)

class fat_cmd () =
object(self)
  val min_fat_bl = flag "--min-fat"
    (Formatted (1e-2, "The minimum branch length for fattened edges (to increase their visibility). To turn off set to 0. Default: %g"))
  val total_width = flag "--total-width"
    (Formatted (400., "Set the total pixel width for all of the branches of the tree. Default: %g"))
  val width_multiplier = flag "--width-multiplier"
    (Plain (0., "Override total-width by directly setting the number of pixels per unit of thing displayed."))
  method specl = [
    float_flag min_fat_bl;
    float_flag total_width;
    float_flag width_multiplier;
  ]

  (* Given an absolute total quantity, come up with a scaling which will make
   * that total. *)
  method private multiplier_of_abs_tot abs_tot =
    assert(abs_tot > 0.);
    match fv width_multiplier with
    | 0. -> (fv total_width) /. abs_tot  (* not set manually *)
    | mw -> mw

  (* Feed the absolute-value total of the given float map to the above method.
   * *)
  method private multiplier_of_float_map m =
    self#multiplier_of_abs_tot
      (IntMap.fold (fun _ v x -> (abs_float v) +. x) m 0.)

  method private spread_short_fat t =
    match fv min_fat_bl with
    | 0. -> t
    | min_bl -> Visualization.spread_short_fat min_bl t

  (* Here every mass is the sand color, and there is no min width for coloring.
   * However, we do throw out the width argument if it's less than 1 to avoid
   * disappearing branches.
   * *)
  method private decor_map_of_mass_map m =
    let multiplier = self#multiplier_of_float_map m in
    let to_decor x =
      if x <= 0. then []
      else begin
        let width = x *. multiplier in
        [Decor.sand] @
          (if width < 1. then []
          else [Decor.width width])
      end
    in
    IntMap.map to_decor m

  method private write_spread_tree ~fname ~tree_name t =
    Phyloxml.named_gtree_to_file ~fname ~tree_name (self#spread_short_fat t)

  method private fat_tree_of_massm decor_t m =
    self#spread_short_fat
      (Decor_gtree.add_decor_by_map decor_t (self#decor_map_of_mass_map m))

end

class heat_cmd () =
object(self)
  inherit fat_cmd () as super_viz
  val gray_black_colors = flag "--gray-black"
    (Plain (false, "Use gray/black in place of red/blue to signify the sign of the coefficient for that edge."))
  val min_width = flag "--min-width"
    (Formatted (1., "Specify the minimum width for a branch to be colored and thickened. Default is %g."))
  method specl =
    super_viz#specl
    @ [
      toggle_flag gray_black_colors;
      float_flag min_width;
    ]

  method private color_of_heat heat =
    if heat >= 0. then Decor.red else Decor.blue

  method private gray_black_of_heat heat =
    if heat >= 0. then Decor.gray 180 else Decor.black

  (* Here we have red and blue for positive and negative, and if it's below
   * min_width then the color and the width argument get thrown out. *)
  method private decor_map_of_float_map m =
    let our_color_of_heat =
      if fv gray_black_colors then self#gray_black_of_heat
      else self#color_of_heat
    in
    let multiplier =
      self#multiplier_of_abs_tot
        (IntMap.fold (fun _ v x -> (abs_float v) +. x) m 0.)
    in
    let to_decor x =
      let width = abs_float (x *. multiplier) in
      if width < fv min_width then []
      else [our_color_of_heat x; Decor.width width]
    in
    IntMap.map to_decor m

  method private heat_tree_of_floatim decor_t m =
    self#spread_short_fat
      (Decor_gtree.add_decor_by_map decor_t (self#decor_map_of_float_map m))

end

class classic_viz_cmd () =
object
  val xml = flag "--xml"
    (Plain (false, "Write phyloXML (with colors) for all visualizations."))
  val show_node_numbers = flag "--node-numbers"
    (Plain (false, "Put the node numbers in where the bootstraps usually go."))

  method specl = [
    toggle_flag xml;
    toggle_flag show_node_numbers;
  ]

  method private fmt =
    if fv xml then Visualization.Phyloxml
    else Visualization.Newick

  method private decor_ref_tree pr =
    let ref_tree = Placerun.get_ref_tree pr in
    Decor_gtree.of_newick_gtree
      (if not (fv show_node_numbers) then
          ref_tree
       else
          (Newick_gtree.make_boot_id ref_tree))
end
