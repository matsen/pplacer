open MapsSets
open Fam_batteries

module Prefs = struct
  type mokaphy_prefs = 
    {
      out_fname: string ref;
      use_pp: bool ref;
      p_exp: float ref;
      weighted: bool ref;
      simple_colors: bool ref;
      gray_black_colors: bool ref;
      white_bg: bool ref;
      gray_level : int ref;
      min_width : float ref;
      max_width : float ref;
      refpkg_path : string ref;
    }
  
  let out_fname         p = !(p.out_fname)
  let use_pp            p = !(p.use_pp)
  let p_exp             p = !(p.p_exp)
  let weighted          p = !(p.weighted)
  let simple_colors     p = !(p.simple_colors)
  let gray_black_colors p = !(p.gray_black_colors)
  let white_bg          p = !(p.white_bg)
  let gray_level        p = !(p.gray_level)
  let min_width         p = !(p.min_width)
  let max_width         p = !(p.max_width)
  let refpkg_path       p = !(p.refpkg_path)
  
  let defaults () = 
    { 
      out_fname = ref "";
      use_pp = ref false;
      p_exp = ref 1.;
      weighted = ref true;
      simple_colors = ref true;
      gray_black_colors = ref false;
      white_bg = ref false;
      gray_level = ref 5;
      min_width = ref 0.5;
      max_width = ref 13.;
      refpkg_path = ref "";
    }
  
  let specl_of_prefs prefs = 
[
"-o", Arg.Set_string prefs.out_fname,
"Set the filename to write to. Otherwise write to stdout.";
"-p", Arg.Set prefs.use_pp,
"Use posterior probability.";
"-c", Arg.Set_string prefs.refpkg_path,
(Mokaphy_common.refpkg_help "heat");
"--exp", Arg.Set_float prefs.p_exp,
"The exponent for the integration, i.e. the value of p in Z_p.";
"--point", Arg.Clear prefs.weighted,
Mokaphy_common.weighted_help;
"--color-gradation", Arg.Clear prefs.simple_colors,
"Use color gradation as well as thickness to represent mass transport.";
"--gray-black", Arg.Set prefs.gray_black_colors,
"Use gray and black in place of red and blue to signify the sign of the KR along that edge.";
"--white-bg", Arg.Set prefs.white_bg,
"Make colors for the heat tree which are compatible with a white background.";
Mokaphy_common.spec_with_default "--gray-level" (fun o -> Arg.Set_int o) prefs.gray_level
"Specify the amount of gray to mix into the color scheme. Default is %d.";
Mokaphy_common.spec_with_default "--min-width" (fun o -> Arg.Set_float o) prefs.min_width
"Specify the minimum width of the branches in a heat tree. Default is %g.";
Mokaphy_common.spec_with_default "--max-width" (fun o -> Arg.Set_float o) prefs.max_width
"Specify the maximum width of the branches in a heat tree. Default is %g.";
]
end


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
  let gray_level = Prefs.gray_level prefs in
  let intensity = intensity_of_heat ~p heat
  and color = simple_color_of_heat heat
  and gray = 
    Decor.gray 
      (if Prefs.white_bg prefs then 
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
  let p = Prefs.p_exp prefs
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
    if Prefs.simple_colors prefs then 
      simple_color_of_heat scaled_heat
    else if Prefs.gray_black_colors prefs then 
      gray_black_of_heat scaled_heat
    else color_of_heat prefs ~p scaled_heat
  in
  let min_width = Prefs.min_width prefs in
  let width_diff = (Prefs.max_width prefs) -. min_width in
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

let heat prefs = function
  | [pr1; pr2] as prl ->
      let fname = match Prefs.out_fname prefs with
        | "" -> (Mokaphy_common.cat_names prl)^".heat.xml"
        | s -> s
      in
      let ref_tree = Placerun.get_same_tree pr1 pr2 
      and is_weighted = Prefs.weighted prefs
      and use_pp = Prefs.use_pp prefs
      in
      let tree_name = Mokaphy_common.chop_suffix_if_present fname ".xml"
      and my_pre_of_pr = Mokaphy_common.pre_of_pr ~is_weighted ~use_pp
      and refpkgo = 
        Mokaphy_common.refpkgo_of_fname (Prefs.refpkg_path prefs) 
      in
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
              Mokaphy_common.make_tax_pre taxt ~is_weighted ~use_pp ti_imap in
            [Some (tree_name^".tax"),
            make_heat_tree prefs taxt 
              (my_make_tax_pre pr1)
              (my_make_tax_pre pr2)]
        end)
  | [] -> () (* e.g. heat -help *)
  | _ -> failwith "Please specify exactly two place files to make a heat tree."

