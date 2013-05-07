open Ppatteries
open Subcommand
open Guppy_cmdobjs

let save_named_fal fname fal =
  Csv.save fname (Guppy_splitify.fal_to_strll fal)

let expand full_length m arr =
  let full = Array.make full_length 0. in
  Array.iteri (fun i v -> full.(IntMap.find i m) <- v) arr;
  full

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
  val som = flag "--som"
    (Formatted (0, "The number of dimensions to rotate for support overlap minimization\
    (default is %d; options are 0, 2, 3)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val symmv = flag "--symmv"
    (Plain (false, "Use a complete eigendecomposition rather than power iteration."))
  val raw_eval = flag "--raw-eval"
    (Plain (false, "Output the raw eigenvalue rather than the fraction of variance."))

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_heat#specl
    @ [
      int_flag write_n;
      int_flag som;
      toggle_flag scale;
      toggle_flag symmv;
    ]
    @ super_splitify#specl

  method desc =
"performs edge principal components"
  method usage = "usage: epca [options] placefiles"

  method private placefile_action prl =
    self#check_placerunl prl;
    let weighting, criterion = self#mass_opts
    and scale = fv scale
    and write_n = fv write_n
    and som = fv som
    and _, t = self#get_rpo_and_tree (List.hd prl)
    and prefix = self#single_prefix ~requires_user_prefix:true () in
    (* data is n x p, i.e. number of samples by number of variables. *)
    let data, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
        |> self#filter_rep_edges prl
    in
    let data, const_reduction_map, const_orig_length =
      self#filter_constant_columns data
    in
    let n_unique_rows = List.length (List.sort_unique compare data) in

    (* Various checks and so on... *)
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
    if som > write_n || not (Array.exists (fun x -> x = som) [|0; 2; 3|]) then
      failwith(Printf.sprintf "Number of components to rotate cannot be greater \
      than write-n, and must be either 0, 2 or 3.");
    let comp_n = max write_n 3 in

    (* Once we have eigenvalues and eigenvectors, this will project the data
     * and write. *)
    let write_results vals vects prefix =
      (* Only want to keep as many of the results as were asked for in --write-n*)
      let write_keep arr = Array.sub arr 0 write_n in
      let (vals, vects) = (write_keep vals, write_keep vects) in
      let combol = (List.combine (Array.to_list vals) (Array.to_list vects))
      and names = (List.map Placerun.get_name prl) in
      let full_combol = List.map
        (Tuple.Tuple2.map2
           (expand const_orig_length const_reduction_map
            %> expand rep_orig_length rep_reduction_map))
        combol
      in
      Phyloxml.named_gtrees_to_file
        (prefix^".xml")
        (List.map
          (fun (vals, vects) ->
            (Some (string_of_float vals),
            super_heat#heat_tree_of_float_arr t vects |> self#maybe_numbered))
          full_combol);
      save_named_fal
        (prefix^".trans")
        (List.map (fun (vals, vects) -> (string_of_float vals, vects)) combol);
      (* Below:
        Take the dot product of each data point with the principal component
        vector. This is the same as multiplying on the right by the matrix
        whose columns are the principal components. *)
      save_named_fal
        (prefix^".proj")
        (List.combine
          names
          (List.map (fun d -> Array.map (Pca.dot d) vects) data));
      save_named_fal
        (prefix^".edgediff")
        (List.combine names data)
    in

    let (vals, vects) = Pca.gen_pca
      ~use_raw_eval:(fv raw_eval)
      ~scale
      ~symmv:(fv symmv)
      comp_n
      (Array.of_list data)
    in
    write_results vals vects prefix;
    if som <> 0 then
      try
        let (rot_vals, rot_vects) = Som.som_rotation vects som vals in
        write_results rot_vals rot_vects (prefix^".som")
      with
      | Som.MinimizationError ->
          Printf.eprintf "There was a problem with the minimization routine. \
          Please either try --som 2 or --som 0\n"
      | e ->
          raise e

end
