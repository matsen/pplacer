open Ppatteries
open Subcommand
open Guppy_cmdobjs

let save_named_fal fname fal =
  Csv.save fname (Guppy_splitify.fal_to_strll fal)

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
  inherit subcommand () as super
  inherit output_cmd ~show_fname:false ~prefix_required:true () as super_output
  inherit mass_cmd () as super_mass
  inherit heat_cmd () as super_heat
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile
  inherit splitify_cmd () as super_splitify

  val write_n = flag "--write-n"
    (Formatted (5, "The number of principal coordinates to write out (default is %d)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val symmv = flag "--symmv"
    (Plain (false, "Use a complete eigendecomposition rather than power iteration."))
  val raw_eval = flag "--raw-eval"
    (Plain (false, "Output the raw eigenvalue rather than the fraction of variance."))
  val length = flag "--length"
    (Plain (false, "'Length PCA'. Experimental."))

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_heat#specl
    @ [
      int_flag write_n;
      toggle_flag scale;
      toggle_flag symmv;
      toggle_flag raw_eval;
      toggle_flag length;
    ]
    @ super_splitify#specl

  method desc =
"performs edge principal components"
  method usage = "usage: pca [options] placefiles"

  method private placefile_action prl =
    self#check_placerunl prl;
    let weighting, criterion = self#mass_opts
    and scale = fv scale
    and write_n = fv write_n
    and _, t = self#get_rpo_and_tree (List.hd prl)
    and prefix = self#single_prefix ~requires_user_prefix:true () in
    let data, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
        |> self#filter_rep_edges prl
    in
    let data, const_reduction_map, const_orig_length =
      self#filter_constant_columns data
    in
    let n_unique_rows = List.length (List.sort_unique compare data) in
    if n_unique_rows <= 2 then
      failwith(Printf.sprintf "You have only %d unique row(s) in your data \
      after transformation. This is not enough to do edge PCA." n_unique_rows);
    let write_n =
      if n_unique_rows < write_n then begin
        Printf.printf "You have only %d unique rows in your data after \
          transformation. Restricting to this number of principal components.\n"
          n_unique_rows;
        n_unique_rows
      end
      else
        write_n
    in
    let faa = Array.of_list data in
    let (eval, evect) =
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
          (if (fv symmv) then Pca.symmv_eigen else Pca.power_eigen) write_n m
        in
        (* If we were just going for the eigenvects of GD then this would be a
         * right multiplication of the inverse of the diagonal matrix d_root.
         * However, according to length PCA we must multiply on the right by d,
         * which ends up just being right multiplication by d_root. *)
        right_diag_mul_va u d_root;
        (l, Array.map Gsl_vector.to_array u)
      end
      else
        Pca.gen_pca ~use_raw_eval:(fv raw_eval)
                    ~scale ~symmv:(fv symmv) write_n faa
    in
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect))
    and names = (List.map Placerun.get_name prl) in
    let full_combol = List.map
      (second
         (expand const_orig_length const_reduction_map
          |- expand rep_orig_length rep_reduction_map))
      combol
    in
    Phyloxml.named_gtrees_to_file
      (prefix^".xml")
      (List.map
        (fun (eval, evect) ->
          (Some (string_of_float eval),
          super_heat#heat_tree_of_float_arr t evect |> self#maybe_numbered))
        full_combol);
    save_named_fal
      (prefix^".rot")
      (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
    save_named_fal
      (prefix^".trans")
      (List.combine
        names
        (List.map (fun d -> Array.map (Pca.dot d) evect) data));
    save_named_fal
      (prefix^".edgediff")
      (List.combine names data);
    ()

end
