open Ppatteries
open Subcommand
open Guppy_cmdobjs

let expand full_length m arr =
  let full = Array.make full_length 0. in
  Array.iteri (fun i v -> full.(IntMap.find i m) <- v) arr;
  full

(* Multiplication of matrices by diagonal vectors on left and right sides. The
 * examples below are based on:
let vd = Gsl_vector.of_array [|1.; 2.; 3.;|];;
let faa = [| [| 2.; 1.; 5.; |]; [| 2.; 4.; 0.; |] |];;
*)

(*
let m = Gsl_matrix.of_arrays faa;;
left_diag_mul_mat vd m;;
- : Gsl_matrix.matrix = {{2.; 1.; 5.}; {4.; 8.; 0.}}
*)
let left_diag_mul_mat vd m =
  for i=0 to (fst (Gsl_matrix.dims m))-1 do
    Gsl_vector.scale (Gsl_matrix.row m i) vd.{i}
  done

(*
let m = Gsl_matrix.of_arrays faa;;
right_diag_mul_mat m vd;;
- : Gsl_matrix.matrix = {{2.; 2.; 15.}; {2.; 8.; 0.}}
*)
let right_diag_mul_mat m vd =
  for i=0 to (fst (Gsl_matrix.dims m))-1 do
    Gsl_vector.mul (Gsl_matrix.row m i) vd
  done

(*
let va = Array.map Gsl_vector.of_array faa;;
right_diag_mul_va va vd;;
- : Gsl_vector.vector array = [|{2.; 2.; 15.}; {2.; 8.; 0.}|]
*)
let right_diag_mul_va va vd =
  Array.iter (fun v -> Gsl_vector.mul v vd) va

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca

  val length = flag "--length"
    (Plain (false, "'Length PCA'. Experimental."))

  method specl =
    super_pca#specl
    @ [
      toggle_flag length;
    ]

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

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data (_, _, const_reduction_map, _) t =
    let faa = Array.of_list data in
    if (fv length) then let open Linear_utils in begin
      let m = Pca.covariance_matrix ~scale faa
      and d = Gsl_vector.create ~init:0. (Array.length faa.(0))
      in
      (* Put together a reduced branch length vector, such that the ith entry
         represents the sum of the branch lengths that get collapsed to the ith
         edge. *)
      IntMap.iter
        (fun red_i orig_i -> d.{red_i} <- d.{red_i} +. Gtree.get_bl t orig_i)
        const_reduction_map;
      (* ppr_gsl_vector Format.std_formatter d; *)
      vec_iter (fun x -> assert(x > 0.)) d;
      (* The trick for diagonalizing matrices of the form GD, where D is
       * diagonal. See diagd.ml for notes. *)
      let d_root = vec_map sqrt d in
      left_diag_mul_mat d_root m;
      right_diag_mul_mat m d_root;
      let (l, u) =
        (if symmv then Pca.symmv_eigen else Pca.power_eigen) write_n m
      in
      (* If we were just going for the eigenvects of GD then this would be a
       * right multiplication of the inverse of the diagonal matrix d_root.
       * However, according to length PCA we must multiply on the right by d,
       * which ends up just being right multiplication by d_root. *)
      right_diag_mul_va u d_root;
      (l, Array.map Gsl_vector.to_array u)
    end
    else
      Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa

  method private post_pca (eval, evect) (rep_reduction_map, rep_orig_length, const_reduction_map, const_orig_length) =
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect)) in
    let full_combol =
      List.map
        (second
           (expand const_orig_length const_reduction_map
               |- expand rep_orig_length rep_reduction_map))
        combol
    in
    (combol, full_combol)

end
