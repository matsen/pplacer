open Ppatteries
open Guppy_cmdobjs
open Lpca

let norm_rows m =
  let n_rows, _ = Gsl_matrix.dims m in
  for i=0 to (n_rows-1) do
    Linear_utils.l2_normalize (Gsl_matrix.row m i);
  done

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
    let fplf, extra = Lpca.gen_fplf sl t in
    (Array.to_list (Gsl_matrix.to_arrays fplf), extra)

  method private gen_pca ~use_raw_eval ~scale ~symmv n_components fplf extra _ =
    let (eval, pre_evect) = Pca.gen_pca ~use_raw_eval ~scale ~symmv n_components (Array.of_list fplf) in
    (* TODO: Erick M.: If you really feel guilty you can do
       http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Bigarray.Array2.html
       slicing and then vector copying.
       http://www.gnu.org/software/gsl/manual/html_node/Copying-vectors.html *)
    let af = Gsl_matrix.of_arrays (Array.of_list (List.map Gsl_vector.to_array (List.of_enum (IntMap.values extra.af))))
    and w' = Gsl_matrix.of_arrays pre_evect in
    let n_edges, _ = Gsl_matrix.dims af in
    let afw = Gsl_matrix.create n_edges n_components in
    Gsl_blas.gemm ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.Trans ~alpha:1. ~a:af ~b:w' ~beta:0. ~c:afw;
    let afw' = Gsl_matrix.create n_components n_edges in
    Gsl_matrix.transpose afw' afw;
    norm_rows afw';
    let evect = Gsl_matrix.to_arrays afw' in
    (eval, evect)

  method private post_pca (eval, evect) _ =
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect)) in
    (combol, combol)

end
