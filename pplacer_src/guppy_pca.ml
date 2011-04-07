open Subcommand
open Guppy_cmdobjs

open MapsSets
open Fam_batteries

let tolerance = 1e-3

let map_of_arr a =
  let m = ref IntMap.empty in
  Array.iteri (fun i x -> m := IntMap.add i x (!m)) a;
  !m

let heat_map_of_floatim multiplier m =
  let min_width = 1. in
  IntMap.map
    (fun v ->
      if v = 0. then []
      else begin
        let width = multiplier *. (abs_float v) in
        (Guppy_kr_heat.simple_color_of_heat v)::
          (if width < min_width then []
           else [Decor.width width])
      end)
    m

let heat_tree_of_floatim multiplier t m =
  Visualization.spread_short_fat 1e-2
    (Decor_gtree.add_decor_by_map t ((heat_map_of_floatim multiplier) m))

let pca_complete ?scale transform
      weighting criterion multiplier write_n refpkgo out_prefix prl =
  let prt = Mokaphy_common.list_get_same_tree prl in
  let t = match refpkgo with
  | None -> Decor_gtree.of_newick_gtree prt
  | Some rp -> Refpkg.get_tax_ref_tree rp
  in
  let data = List.map (Guppy_splitify.splitify_placerun transform weighting criterion) prl
  in
  let (eval, evect) = Pca.gen_pca ?scale ~n_keep:write_n (Array.of_list data)
  in
  let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
  and names = (List.map Placerun.get_name prl)
  in
  Phyloxml.named_gtrees_to_file
    (out_prefix^".xml")
    (List.map
      (fun (eval, evect) ->
        (Some (string_of_float eval),
        heat_tree_of_floatim multiplier t (map_of_arr evect)))
      combol);
  Guppy_splitify.save_named_fal
    (out_prefix^".rot")
    (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
  Guppy_splitify.save_named_fal
    (out_prefix^".trans")
    (List.combine
      names
      (List.map (fun d -> Array.map (Pca.dot d) evect) data));
  Guppy_splitify.save_named_fal
    (out_prefix^".edgediff")
    (List.combine names data);
  ()


class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile

  val write_n = flag "--write-n"
    (Plain (5, "The number of principal coordinates to write out (default is 5)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val multiplier = flag "--multiplier"
    (Formatted (50., "The factor by which we multiply the principal component eigenvectors to get branch thickness. Default: %g."))

  method specl =
    super_out_prefix#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ [
      int_flag write_n;
      toggle_flag scale;
      float_flag multiplier;
    ]

  method desc =
"performs edge principal components"
  method usage = "usage: pca [options] placefiles"

  method private placefile_action prl =
    let transform, weighting, criterion = self#mass_opts in
    self#check_placerunl prl;
    pca_complete
      ~scale:(fv scale)
      transform
      weighting
      criterion
      (fv multiplier)
      (fv write_n)
      (self#get_rpo)
      (fv out_prefix)
      prl
end
