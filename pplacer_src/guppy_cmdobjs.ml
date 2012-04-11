(*
 * This is for the hierarchy of common command objects.
 *)

open Subcommand
open Ppatteries


(* *** general objects *** *)

type outputloc =
  | File of string
  | Directory of string * string
  | Unspecified

class output_cmd ?(show_fname = true) ?(prefix_required = false) () =
object (self)
  val out_fname = flag "-o"
    (Needs_argument ("output file", "Specify the filename to write to."))
  val out_dir = flag "--out-dir"
    (Needs_argument ("output directory", "Specify the directory to write files to."))
  val out_prefix = flag "--prefix"
    (Needs_argument ("output prefix",
                     if prefix_required then
                       "Specify a string to be prepended to filenames. Required."
                     else
                       "Specify a string to be prepended to filenames."))
  method specl =
    let flags = [
      string_flag out_dir;
      string_flag out_prefix;
    ]
    in
    if show_fname then
      string_flag out_fname :: flags
    else
      flags

  method private out_channel =
    match fvo out_fname, fvo out_dir, fvo out_prefix with
      | None, None, None
      | Some "-", None, None -> stdout

      | Some fname, Some dir, Some prefix -> open_out (Filename.concat dir (prefix ^ fname))
      | Some fname, Some dir, None -> open_out (Filename.concat dir fname)
      | Some fname, None, Some prefix -> open_out (prefix ^ fname)
      | Some fname, None, None -> open_out fname

      | None, _, _ -> failwith "-o option is required"

  method private out_file_or_dir ?(default = Directory (".", "")) () =
    match fvo out_fname, fvo out_dir, fvo out_prefix with
      | None, None, None -> default

      | None, Some dir, None -> Directory (dir, "")
      | None, None, Some prefix -> Directory (".", prefix)
      | None, Some dir, Some prefix -> Directory (dir, prefix)

      | Some fname, None, None -> File fname
      | Some fname, None, Some prefix -> File (prefix ^ fname)
      | Some fname, Some dir, None -> File (Filename.concat dir fname)
      | Some fname, Some dir, Some prefix -> File (Filename.concat dir (prefix ^ fname))

  method private single_prefix ?(requires_user_prefix = false) () =
    match self#out_file_or_dir () with
      | Directory (_, "") when requires_user_prefix -> failwith "--prefix option is required"
      | Directory (dir, prefix) -> Filename.concat dir prefix
      | _ -> failwith "-o option is illegal"

  method private single_file ?default () =
    match self#out_file_or_dir ?default () with
      | File fname -> fname
      | _ -> failwith "-o option is required"

end

class tabular_cmd ?(default_to_csv = false) () =
object (self)
  inherit output_cmd () as super_output

  val as_csv =
    if default_to_csv then
      flag "--no-csv"
        (Plain (true, "Output the results as a padded matrix instead of csv."))
    else
      flag "--csv"
        (Plain (false, "Output the results as csv instead of a padded matrix."))

  method specl =
    super_output#specl
  @ [toggle_flag as_csv]

  method private channel_opt ch =
    match ch with
      | Some x -> x
      | None -> self#out_channel

  method private write_csv ?ch data =
    self#channel_opt ch
      |> csv_out_channel
      |> Csv.to_out_obj
      |> flip Csv.output_all data

  method private write_matrix ?ch data =
    self#channel_opt ch
      |> flip String_matrix.write_padded data

  method private write_ll_tab ?ch data =
    if fv as_csv then self#write_csv ?ch data
    else
      List.map Array.of_list data
        |> Array.of_list
        |> self#write_matrix ?ch

  method private write_aa_tab ?ch data =
    if not (fv as_csv) then self#write_matrix ?ch data
    else
      Array.map Array.to_list data
        |> Array.to_list
        |> self#write_csv ?ch

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

  (* This checks to make sure that the placerun given has a reference tree that
   * matches the reference package tree, if it exists. *)
  method private get_rpo_and_tree pr =
    let alt_tree = Decor_gtree.of_newick_gtree pr.Placerun.ref_tree in
    match self#get_rpo with
      | None -> (None, alt_tree)
      | Some rp ->
        Refpkg.pr_check_tree_approx rp pr;
        if Refpkg.tax_equipped rp then (Some rp, Refpkg.get_tax_ref_tree rp)
        else (None, alt_tree)

  method private get_decor_ref_tree =
    let rp = self#get_rp in
    if Refpkg.tax_equipped rp then Refpkg.get_tax_ref_tree rp
    else Decor_gtree.of_newick_gtree (Refpkg.get_ref_tree rp)

  method private decor_ref_tree_from_placerunl prl =
    self#check_placerunl prl;
    List.hd prl |> self#get_rpo_and_tree |> snd

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
    let pr = Placerun_io.of_any_file fname in
    if 0 = Placerun.n_pqueries pr then failwith (fname^" has no placements!");
    placerun_map := SM.add fname pr !placerun_map;
    pr
  end

class virtual placefile_cmd () =
object (self)
  method virtual private placefile_action: 'a Placerun.placerun list -> unit
  method action fnamel =
    fnamel
      |> List.map (Placerun_io.maybe_of_split_file ~getfunc:placerun_by_name)
      |> List.flatten
      |> self#placefile_action

  method private write_placefile fname pr =
    if fname.[0] = '@' then
      let name = Filename.chop_extension
        (String.sub fname 1 ((String.length fname) - 1))
      in
      placerun_map := SM.add fname (Placerun.set_name pr name) !placerun_map
    else
      Placerun_io.to_json_file fname pr

end


(* *** mass and kr-related objects *** *)

class mass_cmd ?(point_choice_allowed = true) () =
object (self)
  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for the weight."))
  val spread = flag "--point-mass"
    (Plain (true, "Treat every pquery as a point mass concentrated on the highest-weight placement."))
  method specl = [
    toggle_flag use_pp;
  ]
    |> if point_choice_allowed then
        toggle_flag spread |> List.cons
      else identity

  method private criterion =
    (if fv use_pp then Placement.post_prob else Placement.ml_ratio)

  method private mass_opts = (
    (if fv spread then Mass_map.Spread else Mass_map.Point),
    self#criterion
  )
end

(* For normalizing by various things related to the tree. *)
class normalization_cmd () =
  let no_normalization _ = 1.
  and tree_length t = Gtree.tree_length t
  in
  let normalization_map =
    StringMap.of_pairlist
      [
        "", no_normalization;
        "tree-length", tree_length;
      ]
  in

object
  val normalize = flag "--normalize"
    (Plain ("", "Divide KR by a given value. Legal arguments are \"tree-length\"."))
  method specl = [ string_flag normalize; ]

  method private get_normalization: <get_bl: float; ..> Gtree.gtree -> float = fun t ->
    let s = fv normalize in
    let f =
      try
        StringMap.find s normalization_map
      with
        | Not_found -> failwith ("Normalization "^s^" not known.")
    in
    f t
end

class kr_cmd () =
  object
  (* normalizations. We can divide by these to get a given perspective on KR. *)
    val p_exp = flag "-p"
      (Formatted (1., "Exponent for KR integration, i.e. value of p in Z_p. Default %g."))
    method specl = [ float_flag p_exp; ]
end


(* *** visualization-related objects *** *)

class numbered_tree_cmd () =
object
  val numbered = flag "--node-numbers"
    (Plain (false, "Put the node numbers in where the bootstraps usually go."))

  method specl = [
    toggle_flag numbered;
  ]

  method private maybe_numbered: (<to_numbered: int -> 'a; ..> as 'a) Gtree.gtree -> 'a Gtree.gtree =
    if fv numbered then
      Gtree.mapi_bark_map (fun i x -> x#to_numbered i)
    else
      identity

end

exception Invalid_abs_tot
class fat_cmd () =
object(self)
  inherit numbered_tree_cmd () as super_numbered_tree

  val min_fat_bl = flag "--min-fat"
    (Formatted (1e-2, "The minimum branch length for fattened edges (to increase their visibility). To turn off set to 0. Default: %g"))
  val total_width = flag "--total-width"
    (Formatted (300., "Set the total pixel width for all of the branches of the tree. Default: %g"))
  val width_multiplier = flag "--width-factor"
    (Plain (0., "Override total-width by directly setting the number of pixels per unit of thing displayed."))
  method specl = [
    float_flag min_fat_bl;
    float_flag total_width;
    float_flag width_multiplier;
  ] @ super_numbered_tree#specl

  (* Given an absolute total quantity, come up with a scaling which will make
   * that total. *)
  method private multiplier_of_abs_tot abs_tot =
    if abs_tot <= 0. then raise Invalid_abs_tot;
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
   * The multiplier_opt allows for overriding the multiplier.
   * *)
  method private decor_map_of_mass_map ?multiplier_override m =
    let multiplier =
      match multiplier_override with
      | None -> self#multiplier_of_float_map m
      | Some m -> m
    in
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

  method private fat_tree_of_massm ?multiplier_override decor_t m =
    self#spread_short_fat
      (Decor_gtree.add_decor_by_map
        decor_t
        (self#decor_map_of_mass_map ?multiplier_override m))

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
    if heat >= 0. then Decor.brew_orange else Decor.brew_green

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

  method private heat_tree_of_float_arr decor_t a =
    self#heat_tree_of_floatim decor_t (Visualization.intmap_of_arr a)

end

class classic_viz_cmd () =
object (self)
  inherit numbered_tree_cmd () as super_numbered_tree
  val xml = flag "--xml"
    (Plain (false, "Write phyloXML (with colors) for all visualizations."))

  method specl = [
    toggle_flag xml;
  ] @ super_numbered_tree#specl

  method private fmt =
    if fv xml then Visualization.Phyloxml
    else Visualization.Newick

  method private decor_ref_tree pr =
    Placerun.get_ref_tree pr
      |> self#maybe_numbered
      |> Decor_gtree.of_newick_gtree

  method private write_trees suffix named_trees = function
    | Directory (dir, prefix) ->
      let prefix = Filename.concat dir prefix in
      List.iter
        (fun (name, trees) ->
          Visualization.trees_to_file
            self#fmt
            (prefix ^ name ^ suffix)
            trees)
        named_trees
    | File fname ->
      Visualization.trees_to_file
        ~with_suffix:false
        self#fmt
        fname
        (List.map snd named_trees |> List.flatten)
    | Unspecified -> ()

end

class sqlite_cmd () =
object
  val sqlite_fname = flag "--sqlite"
    (Needs_argument ("sqlite database", "Specify the database file to use."))
  method specl = [ string_flag sqlite_fname; ]

  method private get_db =
    Sqlite3.db_open (fv sqlite_fname)
end

class splitify_cmd () =

let tolerance = 1e-3
and splitify x = x -. (1. -. x)
and sgn = flip compare 0. |- float_of_int
and arr_of_map default len m =
  Array.init len (fun i -> IntMap.get i default m) in

(* get the mass below the given edge, excluding that edge *)
let below_mass_map edgem t =
  let m = ref IntMap.empty in
  let total =
    Gtree.recur
      (fun i below_massl ->
        let below_tot = List.fold_left ( +. ) 0. below_massl in
        m := IntMap.check_add i below_tot (!m);
        (IntMap.get i 0. edgem) +. below_tot)
      (fun i -> IntMap.get i 0. edgem)
      t
  in
  assert(abs_float(1. -. total) < tolerance);
  !m
in

object (self)
  val kappa = flag "--kappa"
    (Formatted (1., "Specify the exponent for scaling between weighted and unweighted splitification. default: %g"))
  method specl = [float_flag kappa]

  method private splitify_transform =
    let kappa = fv kappa in
    if kappa =~ 0. then
      splitify |- sgn
    else if kappa =~ 1. then
      splitify
    else if kappa < 0. then
      failwith "--kappa must be a non-negative number"
    else
      fun x -> let y = splitify x in sgn y *. abs_float y ** kappa

  (* Take a placerun and turn it into a vector which is indexed by the edges of
   * the tree.
   * Later we may cut the edge mass in half; right now we don't do anything with it. *)
  method private splitify_placerun weighting criterion pr =
    let preim = Mass_map.Pre.of_placerun weighting criterion pr
    and t = Placerun.get_ref_tree pr
    and splitify_fn = self#splitify_transform in
    arr_of_map
      (splitify_fn 0.)
      (1+(Gtree.top_id t))
      (IntMap.map
         splitify_fn
         (below_mass_map (Mass_map.By_edge.of_pre preim) t))

end

class voronoi_cmd () =
object (self)
  inherit tabular_cmd ~default_to_csv:true () as super_tabular
  inherit numbered_tree_cmd () as super_numbered_tree

  val verbose = flag "-v"
    (Plain (false, "If specified, write progress output to stderr."))
  val trimmed_tree_file = flag "-t"
    (Needs_argument ("trimmed tree file", "If specified, the path to write the trimmed tree to."))
  val leaf_cutoff = flag "--leaves"
    (Needs_argument ("leaves", "The maximum number of leaves to keep in the tree."))
  val algorithm = flag "--algorithm"
    (Formatted ("full",
                "Which algorithm to use to prune leaves. \
                 Choices are 'greedy', 'full', 'force', and 'pam'. Default %s."))
  val all_eclds_file = flag "--all-eclds-file"
    (Needs_argument ("", "If specified, write out a csv file containing every intermediate computed ECLD."))
  val soln_log = flag "--log"
    (Needs_argument ("", "If specified with the full algorithm, write out a csv file containing solutions at \
                          every internal node."))
  val always_include = flag "--always-include"
    (Needs_argument ("", "If specified, the leaf names read from the provided file will not be trimmed."))

  method specl =
    super_tabular#specl
  @ super_numbered_tree#specl
  @ [
    toggle_flag verbose;
    string_flag trimmed_tree_file;
    int_flag leaf_cutoff;
    string_flag algorithm;
    string_flag all_eclds_file;
    string_flag soln_log;
    string_flag always_include;
  ]

  method private perform_voronoi ?decor_tree gt mass_cb =
      let leaf_cutoff = fv leaf_cutoff in
      let alg = match fv algorithm with
        | "greedy" -> (module Voronoi.Greedy: Voronoi.Alg)
        | "full" -> (module Voronoi.Full: Voronoi.Alg)
        | "force" -> (module Voronoi.Forced: Voronoi.Alg)
        | "pam" -> (module Voronoi.PAM: Voronoi.Alg)
        | x -> failwith (Printf.sprintf "unknown algorithm: %s" x)
      and keep = match fvo always_include with
        | None -> None
        | Some fname ->
          let name_map = Newick_gtree.leaf_label_map gt
            |> IntMap.enum
            |> Enum.map swap
            |> StringMap.of_enum
          in
          File.lines_of fname
            |> Enum.map
                (fun name -> match StringMap.Exceptionless.find name name_map with
                 | None -> failwith ("no leaf named " ^ name)
                 | Some i -> i)
            |> IntSet.of_enum
            |> some
      and verbose = fv verbose in
      Voronoi.Full.csv_log :=
        fvo soln_log
          |> Option.map (open_out |- csv_out_channel |- Csv.to_out_obj);
      let module Alg = (val alg: Voronoi.Alg) in
      let diagram = Voronoi.of_gtree gt in
      let mass = mass_cb diagram in

      begin match Option.map IntSet.cardinal keep with
        | Some n_include when n_include > leaf_cutoff ->
          failwith
            (Printf.sprintf
               "More leaves specified via --always-include (%d) than --leaves (%d)"
               n_include
               leaf_cutoff)
        | _ -> ()
      end;
      begin match Gtree.n_taxa gt with
        | n_taxa when n_taxa < leaf_cutoff ->
          failwith
            (Printf.sprintf
               "Cannot prune %d leaves from a tree with %d taxa"
               leaf_cutoff
               n_taxa)
        | _ -> ()
      end;

      let solm = Alg.solve
        ?keep
        ~strict:(fvo all_eclds_file |> Option.is_none)
        ~verbose
        gt
        mass
        leaf_cutoff
      in
      if not (IntMap.mem leaf_cutoff solm) then
        failwith
          (Printf.sprintf "no solution with cardinality %d found; only solutions on the range [%d, %d]"
             leaf_cutoff
             (IntMap.min_binding solm |> fst)
             (IntMap.max_binding solm |> fst));
      let {Voronoi.leaves} = IntMap.find leaf_cutoff solm in
      let cut_leaves = gt
        |> Gtree.leaf_ids
        |> IntSet.of_list
        |> flip IntSet.diff leaves
      in

      begin match fvo trimmed_tree_file with
        | Some fname ->
          decor_tree
            |> Option.default (Decor_gtree.of_newick_gtree gt)
            |> Decor_gtree.color_clades_above cut_leaves
            |> self#maybe_numbered
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
        |> Enum.map (Gtree.get_node_label gt |- flip List.cons [])
        |> List.of_enum
        |> self#write_ll_tab;

end
