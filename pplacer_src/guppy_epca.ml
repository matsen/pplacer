open Ppatteries
open Subcommand
open Guppy_cmdobjs

let expand full_length m arr =
  let full = Array.make full_length 0. in
  Array.iteri (fun i v -> full.(IntMap.find i m) <- v) arr;
  full

type epca_result = { eval: float array; evect: float array array }

type epca_data = { edge_diff: float array list;
                   rep_reduction_map: int IntMap.t;
                   rep_orig_length: int;
                   const_reduction_map: int IntMap.t;
                   const_orig_length: int }

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca
  inherit splitify_cmd () as super_splitify

  method specl =
    super_pca#specl
  @ super_splitify#specl

  method desc =
    "performs edge principal components"
  method usage = "usage: epca [options] placefiles"

  method private prep_data prl =
    let weighting, criterion = self#mass_opts in
    (* edge_diff is n x p, i.e. number of samples by number of variables. *)
    let edge_diff, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
                                    |> self#filter_rep_edges prl
    in
    let edge_diff, const_reduction_map, const_orig_length =
      self#filter_constant_columns edge_diff
    in
    { edge_diff; rep_reduction_map; rep_orig_length; const_reduction_map; const_orig_length }

  method private check_data data write_n =
    let fal = data.edge_diff in
    self#check_uniqueness fal write_n

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data _ =
    let faa = Array.of_list data.edge_diff in
    let (eval, evect) = Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa in
    { eval; evect }

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
      let full_combol = List.map
        (Tuple.Tuple2.map2
           (expand data.const_orig_length data.const_reduction_map
             %> expand data.rep_orig_length data.rep_reduction_map))
        combol
      in
      Phyloxml.named_gtrees_to_file
        (prefix^".xml")
        (List.map
           (fun (vals, vects) ->
             (Some (string_of_float vals),
              self#heat_tree_of_float_arr t vects |> self#maybe_numbered))
           full_combol);
      Guppy_pca.save_named_fal
        (prefix^".trans")
        (List.map (fun (vals, vects) -> (string_of_float vals, vects)) combol);
      (* Below:
         Take the dot product of each data point with the principal component
         vector. This is the same as multiplying on the right by the matrix
         whose columns are the principal components. *)
      Guppy_pca.save_named_fal
        (prefix^".proj")
        (List.combine
           names
           (List.map (fun d -> Array.map (Pca.dot d) vects) data.edge_diff));
      Guppy_pca.save_named_fal
        (prefix^".edgediff")
        (List.combine names data.edge_diff)
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
