open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

type prefs = {
  gray_level: int;
  white_bg: bool;
  p_exp: float;
  simple_colors: bool;
  gray_black_colors: bool;
  min_width: float;
  max_width: float;
}

(* color utils *)

(* intesity is a float from 0 to 1 which is the absolute-valued and
 * exponentiated version of the heat *)
let intensity_of_heat ~p heat = (abs_float heat) ** p

let assert_intensity intensity =
  assert(intensity >= 0. || intensity <= 1.)

let simple_color_of_heat heat =
  if heat >= 0. then Decor.red else Decor.blue

let gray_black_of_heat heat =
  if heat >= 0. then Decor.gray 180 else Decor.black

let color_of_heat prefs ?(p=1.) heat =
  let gray_level = prefs.gray_level in
  let intensity = intensity_of_heat ~p heat
  and color = simple_color_of_heat heat
  and gray =
    Decor.gray
      (if prefs.white_bg then
        255-gray_level
      else
        gray_level)
  in
  assert_intensity intensity;
  Decor.color_avg intensity color gray

let width_value_of_heat ~width_diff ?(p=1.) heat =
  let intensity = intensity_of_heat ~p heat in
  assert_intensity intensity;
  width_diff *. intensity

let color_map prefs t pre1 pre2 =
  let transform = Mass_map.transform_of_str "" in
  let p = prefs.p_exp
  and kr_map =
    IntMap.map
    (* we don't care about where we are along the edge *)
      (List.map snd)
      (Kr_distance.make_kr_map
        (Mass_map.Indiv.of_pre transform pre1)
        (Mass_map.Indiv.of_pre transform pre2)) in
  let sum_over_krs_of_id id =
    List.fold_right
      (fun kr_v -> ( +. ) (kr_v.(0) -. kr_v.(1)))
      (Base.get_from_list_intmap id kr_map)
  in
  let heat_list =
    Stree.recur_listly
      (fun id below ->
        ((id,
          sum_over_krs_of_id
            id
(* the first item a list from a subtree is the total of all of the heat in that
* subtree. therefore to get the total heat for our tree, we just have to total
* all of those *)
            (List.fold_left
              (fun accu -> function
                | (_,heat)::_ -> heat +. accu
                | [] -> accu)
              0.
              below)))
        :: (List.flatten below))
      (Gtree.get_stree t)
  in
  let heat_only = List.map snd heat_list in
  let top_heat = List.hd heat_only in
  if top_heat > Kr_distance.tol then
    raise (Kr_distance.Total_kr_not_zero top_heat);
  (* why do I do it like this rather than mapping abs first? *)
  let max_abs_heat =
    max
      (ListFuns.complete_fold_left max heat_only)
      (-. (ListFuns.complete_fold_left min heat_only))
  in
  let our_color_of_heat scaled_heat =
    if prefs.simple_colors then
      simple_color_of_heat scaled_heat
    else if prefs.gray_black_colors then
      gray_black_of_heat scaled_heat
    else color_of_heat prefs ~p scaled_heat
  in
  let min_width = prefs.min_width in
  let width_diff = prefs.max_width -. min_width in
  IntMapFuns.of_pairlist
    (List.map
      (fun (id, raw_heat) ->
        let scaled_heat = raw_heat /. max_abs_heat in
        let wv = width_value_of_heat ~width_diff ~p scaled_heat in
        (id,
        if wv = 0. then []
        else
          ( our_color_of_heat scaled_heat ) ::
          ( if wv < min_width then []
            else [ Decor.width wv ])))
      heat_list)

let make_heat_tree prefs decor_t pre1 pre2 =
  Decor_gtree.add_decor_by_map
    decor_t
    (color_map prefs decor_t pre1 pre2)



(* The commands *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd () as super_refpkg
  inherit kr_cmd () as super_kr
  inherit placefile_cmd () as super_placefile

  val outfile = flag "-o"
    (Plain ("", "Output file. Default is derived from the input filenames."))
  val simple_colors = flag "--color-gradiation"
    (Plain (true, "Use color gradation as well as thickness to represent mass transport."))
  val gray_black_colors = flag "--gray-black"
    (Plain (false, "Use gray and black in place of red and blue to signify the sign of the KR along that edge."))
  val white_bg = flag "--white-bg"
    (Plain (false, "Make colors for the heat tree which are compatible with a white background."))
  val gray_level = flag "--gray-level"
    (Formatted (5, "Specify the amount of gray to mix into the color scheme. Default is %d."))
  val min_width = flag "--min-width"
    (Formatted (0.5, "Specify the minimum width of the branches in a heat tree. Default is %g."))
  val max_width = flag "--max-width"
    (Formatted (13., "Specify the maximum width of the branches in a heat tree. Default is %g."))

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_kr#specl
    @ [
      string_flag outfile;
      toggle_flag simple_colors;
      toggle_flag gray_black_colors;
      toggle_flag white_bg;
      int_flag gray_level;
      float_flag min_width;
      float_flag max_width;
    ]

  method desc = ""
  method usage = ""

  method private placefile_action = function
    | [pr1; pr2] as prl ->
      let fname = match fv outfile with
        | "" -> (Mokaphy_common.cat_names prl)^".heat.xml"
        | s -> s
      in
      let ref_tree = Placerun.get_same_tree pr1 pr2
      and _, weighting, criterion = self#mass_opts
      in
      let tree_name = Mokaphy_common.chop_suffix_if_present fname ".xml"
      and my_pre_of_pr = Mass_map.Pre.of_placerun weighting criterion
      and refpkgo =
        Mokaphy_common.refpkgo_of_fname (fv refpkg_path)
      in
      let prefs = {
        gray_level = fv gray_level;
        white_bg = fv white_bg;
        p_exp = fv p_exp;
        simple_colors = fv simple_colors;
        gray_black_colors = fv gray_black_colors;
        min_width = fv min_width;
        max_width = fv max_width;
      } in
      Mokaphy_common.check_refpkgo_tree ref_tree refpkgo;
      Phyloxml.named_gtrees_to_file
        fname
        ([Some tree_name,
          make_heat_tree prefs
            (match refpkgo with
            | None -> Decor_gtree.of_newick_gtree ref_tree
            | Some rp -> Refpkg.get_tax_ref_tree rp)
            (my_pre_of_pr pr1)
            (my_pre_of_pr pr2)]
        @ match refpkgo with
        | None -> []
        | Some rp -> begin
            let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
            let my_make_tax_pre =
              Mokaphy_common.make_tax_pre taxt weighting criterion ti_imap in
            [Some (tree_name^".tax"),
            make_heat_tree prefs taxt
              (my_make_tax_pre pr1)
              (my_make_tax_pre pr2)]
        end)
  | [] -> () (* e.g. heat -help *)
  | _ -> failwith "Please specify exactly two place files to make a heat tree."
end
