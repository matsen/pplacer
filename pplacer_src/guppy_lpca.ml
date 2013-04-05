open Ppatteries
open Guppy_cmdobjs
open Lpca

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
    let result, post_data = Lpca.gen_lpca sl t in
    let data = Array.to_list (Gsl_matrix.to_arrays result) in
    (data, post_data)

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data _ _ =
    let faa = Array.of_list data in
    Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa

  method private post_pca (eval, evect) post_data =
    (* TODO: Erick M.: If you really feel guilty you can do
       http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Bigarray.Array2.html
       slicing and then vector copying.
       http://www.gnu.org/software/gsl/manual/html_node/Copying-vectors.html *)
    let w = Gsl_matrix.of_arrays evect
    and af = Lpca.mat_print "AF" (Gsl_matrix.of_arrays (Array.of_list (List.map Gsl_vector.to_array post_data.af)))
    in
    let n_edges, n_samples = Gsl_matrix.dims af in
    let afw = Gsl_matrix.create n_edges n_samples in
    Gsl_blas.gemm ~ta:Gsl_blas.NoTrans ~tb:Gsl_blas.NoTrans ~alpha:1. ~a:af ~b:w ~beta:0. ~c:afw;
    let evect = Gsl_matrix.to_arrays afw in
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect)) in
    (combol, combol)

end
