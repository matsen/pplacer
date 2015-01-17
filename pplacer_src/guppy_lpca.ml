open Ppatteries
open Subcommand
open Guppy_cmdobjs
open Lpca

(* Normalize the rows of a matrix m to unit vectors. *)
let norm_rows m =
  let n_rows, _ = Gsl.Matrix.dims m in
  for i=0 to (n_rows-1) do
    Linear_utils.l2_normalize (Gsl.Matrix.row m i);
  done

(* Compute the n largest singular values of a matrix m (or its transpose) as
   the square root of the eigenvalues of (m* m), where m* is the conjugate
   transpose of m. We assume m is real-valued and therefore compute m* as m'
   for simplicity. *)
let singular_values ~trans m n =
  let real c =
    let open Gsl.Gsl_complex in
    c.re
  in
  let n_rows, n_cols = Gsl.Matrix.dims m in
  let n_mm, ta, tb =
    if trans then (n_rows, Gsl.Blas.NoTrans, Gsl.Blas.Trans) else (n_cols, Gsl.Blas.Trans, Gsl.Blas.NoTrans)
  in
  let mm = Gsl.Matrix.create n_mm n_mm in
  Gsl.Blas.gemm ~ta ~tb ~alpha:1. ~a:m ~b:m ~beta:0. ~c:mm;
  let eval_c = Gsl.Eigen.nonsymm ~protect:false (`M(mm)) in
  let eval = Array.map real (Gsl.Vector_complex.to_array eval_c) in
  Array.sort (flip compare) eval;
  Array.map sqrt (Array.sub eval 0 n)

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca
  inherit splitify_cmd () as super_splitify

  method specl =
    super_pca#specl
  @ super_splitify#specl

  method desc =
    "performs length principal components"
  method usage = "usage: lpca [options] placefiles"

  method private prep_data prl =
    let weighting, criterion = self#mass_opts in
    let sl =
      List.map
        (fun pr ->
          Mass_map.Indiv.sort (Mass_map.Indiv.of_placerun weighting criterion pr))
        prl
    and t = self#get_ref_tree prl in
    Lpca.gen_data sl t

  method private check_data data write_n =
    let fal = Array.to_list (Gsl.Matrix.to_arrays data.fplf) in
    self#check_uniqueness fal write_n

  method private gen_pca ~use_raw_eval ~scale ~symmv n_components data _ =
    let (eval, evect) = Pca.gen_pca ~use_raw_eval ~scale ~symmv n_components (Gsl.Matrix.to_arrays data.fplf) in
    (* Populate the edge-averaged e x s matrix AF from its
       IntMap encoding. See eq:f_tilde. *)
    let af = Gsl.Matrix.of_arrays (Array.of_list (List.map Gsl.Vector.to_array (List.of_enum (IntMap.values data.af))))
    and w' = Gsl.Matrix.of_arrays evect in
    let n_edges, _ = Gsl.Matrix.dims af in
    let afw = Gsl.Matrix.create n_edges n_components in
    (* Compute the edge-averaged eigenvectors. *)
    Gsl.Blas.gemm ~ta:Gsl.Blas.NoTrans ~tb:Gsl.Blas.Trans ~alpha:1. ~a:af ~b:w' ~beta:0. ~c:afw;
    let afw' = Gsl.Matrix.create n_components n_edges in
    Gsl.Matrix.transpose afw' afw;
    (* Normalize the eigenvectors and the edge-averaged eigenvectors to unit
       length. *)
    norm_rows w';
    norm_rows afw';
    (* We want to compute U'Fw, where the columns of w are the eigenvectors of
       F'LF (and therefore Fw are the eigenvectors of GL). For the sake of
       computational efficiency, though, we're accumulating U'F as we traverse
       the tree, and therefore can't normalize the transformed eigenvectors Fw
       before projecting the sample data in U. Luckily we can find the scalars
       needed to normalize the results as the ratio of the singular values of
       U'F to those of U'. *)
    (*
      let weighting, criterion = self#mass_opts in
      let sufa = singular_values ~trans:false data.ufl n_components
      and sua = singular_values ~trans:true (Gsl.Matrix.of_arrays (Array.of_list (List.map (self#splitify_placerun_nx weighting criterion) prl))) n_components in
      let sfa = Array.map2 (/.) sufa sua in
      Array.iteri (fun i x -> Gsl.Vector.scale (Gsl.Matrix.row w' i) (1. /. x)) sfa;
    *)
    let norm_evect = Gsl.Matrix.to_arrays w'
    and edge_evect = Gsl.Matrix.to_arrays afw' in
    { eval; evect = norm_evect; edge_evect }

  method private post_pca result data prl =
    let write_n = fv write_n
    and som = fv som
    and t = self#get_ref_tree prl
    and prefix = self#single_prefix ~requires_user_prefix:true () in
    (* Various checks and so on... *)
    if som > write_n || not (Array.exists (fun x -> x = som) [|0; 2; 3|]) then
      failwith (Printf.sprintf "Number of components to rotate cannot be greater \
      than write-n, and must be either 0, 2 or 3.");

    (* Once we have eigenvalues and eigenvectors, this will project the data
     * and write. *)
    let write_results vals vects prefix =
      (* Only want to keep as many of the results as were asked for in --write-n *)
      let write_keep arr = Array.sub arr 0 write_n in
      let (vals, vects) = (write_keep vals, write_keep vects) in
      let combol = (List.combine (Array.to_list vals) (Array.to_list vects))
      and names = (List.map Placerun.get_name prl) in
      let edge_combol = (List.combine (Array.to_list result.eval) (Array.to_list result.edge_evect))
      in
      Phyloxml.named_gtrees_to_file
        (prefix^".xml")
        (List.map
           (fun (vals, vects) ->
             (Some (string_of_float vals),
              self#heat_tree_of_float_arr t vects |> self#maybe_numbered))
           edge_combol);
      Guppy_pca.save_named_fal
        (prefix^".trans")
        (List.map (fun (vals, vects) -> (string_of_float vals, vects)) combol);
      (*
        Guppy_pca.save_named_fal
        (prefix^".edgetrans")
        (List.map (fun (eval, evect) -> (string_of_float eval, evect)) edge_combol)
      *)
      (* Below:
         Take the dot product of each aggregated inner term of eq:ufl with the
         principal component vector. This is the same as multiplying on the
         right by the matrix whose columns are the principal components. *)
      Guppy_pca.save_named_fal
        (prefix^".proj")
        (List.combine
           names
           (List.map (fun d -> Array.map (Pca.dot d) vects) (Array.to_list (Gsl.Matrix.to_arrays data.ufl))))
    in

    write_results result.eval result.evect prefix;
    if som <> 0 then
      try
        let (rot_vals, rot_vects) = Som.som_rotation result.evect som result.eval in
        write_results rot_vals rot_vects (prefix^".som")
      with
        | Som.MinimizationError ->
          Printf.eprintf "There was a problem with the minimization routine. \
          Please either try --som 2 or --som 0\n"
        | e ->
          raise e

end
