(* calculate the distal and proximal likelihood vectors at each site of the
 * reference tree.
 *)

open Ppatteries
open Stree

(* below 2^-50 = 1e-15 we pull out the exponent into the int *)
let min_allowed_twoexp = -50

module Make (Model: Glvm.Model) =
struct
  module Glv = Model.Glv
  module Glv_arr = Glv_arr.Make(Model)

  (* make a map which goes from node number to the associated likelihood vector
   * for each named node (generally these are assumed to be the leaves) *)
  let like_aln_map_of_data seq_type align tree =
    let like_aln = Alignment.like_aln_of_align seq_type align in
    IntMap.map
      (Array.get like_aln)
      (Alignment.make_aln_index_map
         (Bark_map.to_name_map (Gtree.get_bark_map tree))
         (Array.map fst align))

  let glv_arr_for model tree n_sites =
    Glv_arr.make
      model
      ~n_glvs:(1 + Gtree.n_edges tree)
      ~n_sites

  let calc_distal_and_evolv_dist model tree like_aln_map
      ~distal_glv_arr ~evolv_dist_glv_arr =
    (* calc returns the evolv_dist for each subtree in a list *)
    let rec calc t =
      let id = Stree.top_id t in
      let distal = Glv_arr.get distal_glv_arr id
      and evolv_dist = Glv_arr.get evolv_dist_glv_arr id
      in
      (* first calculate distal *)
      let () = match t with
        | Stree.Node(_, tL) -> begin
          (* take the product of the below *)
          Glv.listwise_prod distal (List.map calc tL);
          Glv.perhaps_pull_exponent min_allowed_twoexp distal
        end
        | Stree.Leaf _ ->
          (* for a leaf, distal is just the LV from the aln for each rate *)
          Glv.prep_constant_rate_glv_from_lv_arr
            distal
            (IntMap.find id like_aln_map)
      in
      (* now calculate evolv_dist *)
      Model.evolve_into
        model ~dst:evolv_dist ~src:distal (Gtree.get_bl tree id);
      evolv_dist
    in
    let _ = calc (Gtree.get_stree tree) in ()

  (* pull out the glv corresponding to the id of the given tree *)
  let glv_from_stree glv_arr t = Glv_arr.get glv_arr (Stree.top_id t)

  let calc_proximal model tree
      evolved_prox ~evolv_dist_glv_arr ~proximal_glv_arr =
    (* calc calculates the proximal vectors. in contrast to the previous calc, the
     * recursion step is actually calculating the proximal vectors for the edges
     * below the given edge.
     *)
    let rec calc = function
      | Stree.Node(id, tL) ->
        let proximal = Glv_arr.get proximal_glv_arr id in
        Model.evolve_into
          model ~dst:evolved_prox ~src:proximal (Gtree.get_bl tree id);
        List.iter
          (fun (chosen, rest) ->
            let prox_below = glv_from_stree proximal_glv_arr chosen in
            Glv.listwise_prod
              prox_below
              (evolved_prox::
                 (List.map (glv_from_stree evolv_dist_glv_arr) rest));
            Glv.perhaps_pull_exponent min_allowed_twoexp prox_below)
          (ListFuns.pull_each_out tL);
        List.iter calc tL
      | Stree.Leaf _ -> ()
    in
    let stree = Gtree.get_stree tree in
    let top_prox = glv_from_stree proximal_glv_arr stree in
    Glv.set_unit top_prox;
    calc stree

  (* Get all of the internal partial likelihood vectors. The utils are just data
   * structures of the same size as the rest we can use for storing things. *)
  let calc_distal_and_proximal model tree like_aln_map util_glv
      ~distal_glv_arr ~proximal_glv_arr ~util_glv_arr =
    calc_distal_and_evolv_dist model tree like_aln_map
      ~distal_glv_arr ~evolv_dist_glv_arr:util_glv_arr;
    calc_proximal model tree util_glv
      ~evolv_dist_glv_arr:util_glv_arr ~proximal_glv_arr;
    ()

  (* Calculate the likelihood of the tree given the model and the data. *)
  let site_log_like_arr model tree like_aln_map util_glv_arr_1 util_glv_arr_2 =
    let almost_top_node_ids = function
      | Stree.Node(_,tL) -> List.map Stree.top_id tL
      | _ -> failwith "can't calculatate likelihood of a leaf!"
    in
    calc_distal_and_evolv_dist model tree like_aln_map
      ~distal_glv_arr:util_glv_arr_1 ~evolv_dist_glv_arr:util_glv_arr_2;
    let ed = util_glv_arr_2
    and util_glv = util_glv_arr_1.(0)
    in
    match almost_top_node_ids (Gtree.get_stree tree) with
    | [i; j] ->
        Glv.set_unit util_glv;
        Model.site_log_like_arr3 model ed.(i) ed.(j) util_glv
    | [i; j; k] -> Model.site_log_like_arr3 model ed.(i) ed.(j) ed.(k)
    | i::j::l ->
        Glv.listwise_prod util_glv (List.map (fun k -> ed.(k)) l);
        Model.site_log_like_arr3 model ed.(i) ed.(j) util_glv
    | _ -> failwith "site_log_like_arr: degree two node"
end

