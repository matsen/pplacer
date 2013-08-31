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
    let edge_diff, rep_reduction_map, rep_orig_length =
      List.map (self#splitify_placerun weighting criterion) prl
                                    |> self#filter_rep_edges prl
    in
    let edge_diff, const_reduction_map, const_orig_length =
      self#filter_constant_columns edge_diff
    in
    { edge_diff; rep_reduction_map; rep_orig_length; const_reduction_map; const_orig_length }

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data _ =
    let faa = Array.of_list data.edge_diff in
    let (eval, evect) = Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa in
    { eval; evect }

  method private post_pca result data prl =
    let combol = (List.combine (Array.to_list result.eval) (Array.to_list result.evect)) in
    let full_combol =
      List.map
        (second
           (expand data.const_orig_length data.const_reduction_map
               |- expand data.rep_orig_length data.rep_reduction_map))
        combol
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
