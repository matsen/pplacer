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

type epca_result = { eval: float array; evect: float array array }

(*
type epca_data = { edge_diff: float array list;
                   rep_reduction_map: int IntMap.t;
                   rep_orig_length: int;
                   const_reduction_map: int IntMap.t;
                   const_orig_length: int }
*)

type epca_data = { edge_diff: float array list }

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca
  inherit splitify_cmd () as super_splitify

  val length = flag "--length"
    (Plain (false, "'Length PCA'. Experimental."))

  method specl =
    super_pca#specl
    @ [
      toggle_flag length;
    ]
    @ super_splitify#specl

  method desc =
    "performs edge principal components"
  method usage = "usage: epca [options] placefiles"

  (* get the mass below the given edge, NOT excluding that edge *)
  method private below_mass_map_no_exclude edgem t =
    let tolerance = 1e-3 in
    let m = ref IntMap.empty in
    let total =
      Gtree.recur
        (fun i below_massl ->
          let below_tot = List.fold_left ( +. ) 0. below_massl in
          let on_tot = (IntMap.get i 0. edgem) +. below_tot in
          m := IntMap.check_add i on_tot (!m);
          on_tot)
        (fun i ->
          let on_tot = IntMap.get i 0. edgem in
          m := IntMap.add i on_tot (!m);
          on_tot)
        t
    in
    assert(abs_float(1. -. total) < tolerance);
    !m

  method private splitify_placerun_no_exclude weighting criterion pr =
    let preim = Mass_map.Pre.of_placerun weighting criterion pr
    and t = Placerun.get_ref_tree pr
    and splitify_fn x = (1. -. x) -. x
    and arr_of_map default len m =
      Array.init len (fun i -> IntMap.get i default m) in
    let splitify_m =
      IntMap.map
        splitify_fn
        (self#below_mass_map_no_exclude (Mass_map.By_edge.of_pre preim) t)
    in
    let a =
      arr_of_map
        (splitify_fn 0.)
        (*(1+(Gtree.top_id t))*)
        (Gtree.top_id t)
        splitify_m in
    a

  method private prep_data prl =
    let weighting, criterion = self#mass_opts in
    let spr_fn exclude =
      if exclude then self#splitify_placerun else self#splitify_placerun_no_exclude
    in
    (* use the original exclusionary splitify only if we're not doing pmlpca *)
    let edge_diff = List.map (spr_fn (not (fv length)) weighting criterion) prl in
    { edge_diff }

(*
    let edge_diff, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
                                    |> self#filter_rep_edges prl
    in
    let edge_diff, const_reduction_map, const_orig_length =
      self#filter_constant_columns edge_diff
    in
    { edge_diff; rep_reduction_map; rep_orig_length; const_reduction_map; const_orig_length }
*)

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data prl =
    let faa = Array.of_list data.edge_diff in
    if (fv length) then let open Linear_utils in begin
      let faa_z = Gsl_matrix.of_arrays faa in
      let n_samples, n_edges = Gsl_matrix.dims faa_z in
      let tmp = Gsl_matrix.create n_edges n_samples in
      Gsl_matrix.transpose tmp faa_z;
      for i=0 to n_edges-1 do
        let col = Gsl_matrix.row tmp i in
        Gsl_vector.add_constant col (-. Lpca.vec_mean col);
      done;
      Gsl_matrix.transpose faa_z tmp;
      let inv_sqrt_smo = 1. /. (sqrt (float (n_samples - 1))) in
      Gsl_matrix.scale faa_z inv_sqrt_smo;
      let faa = Gsl_matrix.to_arrays faa_z in
      let m = Pca.covariance_matrix ~scale faa
      and d = Gsl_vector.create ~init:0. n_edges
      and ref_tree = self#get_rpo_and_tree (List.hd prl) |> snd in
      (* Put together a reduced branch length vector, such that the ith entry
         represents the sum of the branch lengths that get collapsed to the ith
         edge. *)
      (*
      IntMap.iter
        (fun red_i orig_i -> d.{red_i} <- d.{red_i} +. (Gtree.get_bl ref_tree orig_i))
        data.const_reduction_map;
      vec_iter (fun x -> assert(x > 0.)) d;
      *)
      for i=0 to n_edges-1 do
        d.{i} <- (Gtree.get_bl ref_tree i);
      done;
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
      { eval = l; evect = Array.map Gsl_vector.to_array u }
    end
    else
      let (eval, evect) = Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa in
      { eval; evect }

  method private post_pca result data prl =
    let combol = (List.combine (Array.to_list result.eval) (Array.to_list result.evect)) in
    let full_combol = combol
(*
    let full_combol =
      List.map
        (second
           (expand data.const_orig_length data.const_reduction_map
               |- expand data.rep_orig_length data.rep_reduction_map))
        combol
*)
    and prefix = self#single_prefix ~requires_user_prefix:true ()
    and ref_tree = self#get_rpo_and_tree (List.hd prl) |> snd
    and names = List.map Placerun.get_name prl in
    Phyloxml.named_gtrees_to_file
      (prefix^".xml")
      (List.map
         (fun (eval, evect) ->
           (Some (string_of_float eval),
            self#heat_tree_of_float_arr ref_tree evect |> self#maybe_numbered))
         full_combol);
    Guppy_pca.save_named_fal
      (prefix^".rot")
      (List.map (fun (eval, evect) -> (string_of_float eval, evect)) combol);
    Guppy_pca.save_named_fal
      (prefix^".trans")
      (List.combine
         names
         (List.map (fun d -> Array.map (Pca.dot d) result.evect) data.edge_diff));
    Guppy_pca.save_named_fal
      (prefix^".edgediff")
      (List.combine names data.edge_diff)

end
