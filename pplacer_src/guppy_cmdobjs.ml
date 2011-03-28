(*
 * This is for the hierarchy of common command objects.
 *)

open Subcommand

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

class viz_cmd () =
object
  val white_bg = flag "--white-bg"
    (Plain (false, "Make colors appropriate for a white background."))
  val min_fat_bl = flag "--min-fat"
    (Formatted (1e-2, "The minimum branch length for fattened edges (to increase their visibility). To turn off set to 0. Default: %g"))
  val total_width = flag "--total-width"
    (Formatted (400., "Set the total number of pixels for all of the mass. Default: %g"))
  val unit_width = flag "--unit-width"
    (Plain (0., "Set the number of pixels for a single placement (will override total-width if set)."))
  method specl = [
    toggle_flag white_bg;
    float_flag min_fat_bl;
    float_flag total_width;
    float_flag unit_width;
  ]
end

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

class kr_cmd () =
object
  val p_exp = flag "-p"
    (Plain (1., "The exponent for the integration, i.e. the value of p in Z_p."))
  val normalization = flag "--normalization"
    (Plain ("", "Divide KR by a given value. Legal arguments are \"tree-length\"."))
  method specl = [ float_flag p_exp; string_flag normalization; ]
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
