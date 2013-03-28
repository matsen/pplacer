open Ppatteries
open Subcommand
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca

  method specl =
    super_pca#specl

  method desc =
    "performs length principal components"
  method usage = "usage: lpca [options] placefiles"

  method private prep_data prl =
    let weighting, criterion = self#mass_opts
    and _, t = self#get_rpo_and_tree (List.hd prl)
    in
    let sl =
      List.map
        (fun pr ->
          Mass_map.Indiv.sort (Mass_map.Indiv.of_placerun weighting criterion pr))
        prl
    in
    let result = Lpca.mat_print "F'LF" (Lpca.gen_lpca sl t) in
    (* FIXME: at this point result is an upper-triangular matrix; the values
       need to be replicated into the lower triangle before proceeding *)
    let data = Array.to_list (Gsl_matrix.to_arrays result) in
    (* TODO: maybe we can use self#filter_rep_edges too, just as in lPCA, since
       it operates on a float array list? *)
    let data, const_reduction_map, const_orig_length =
      self#filter_constant_columns data in
    (data, (const_reduction_map, const_orig_length))

  method private expand_combol combol (const_reduction_map, const_orig_length) =
    List.map
      (second
         (Guppy_pca.expand const_orig_length const_reduction_map))
      combol

end
