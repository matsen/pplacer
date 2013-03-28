open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca

  method specl =
    super_pca#specl

  method desc =
    "performs edge principal components"
  method usage = "usage: epca [options] placefiles"

  method private prep_data prl =
    let weighting, criterion = self#mass_opts
    in
    let data, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
                               |> self#filter_rep_edges prl
    in
    let data, const_reduction_map, const_orig_length =
      self#filter_constant_columns data
    in
    (data, (rep_reduction_map, rep_orig_length, const_reduction_map, const_orig_length))

  method private expand_combol combol (rep_reduction_map, rep_orig_length, const_reduction_map, const_orig_length) =
    List.map
      (second
         (Guppy_pca.expand const_orig_length const_reduction_map
             |- Guppy_pca.expand rep_orig_length rep_reduction_map))
      combol

end
