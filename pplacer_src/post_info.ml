(* Code for calculating information concerning posterior probability of bases at
 * internal positions of the tree.
 *
 * This code is a bit rugged at the moment-- still figuring out exactly what we
 * want here. It may well disappear in the future.
 *)
open Batteries
open Fam_batteries
open MapsSets

let underscoreize s =
  let s' = String.copy s in
  for i=0 to (String.length s')-1 do
    if s'.[i] = ' ' then s'.[i] <- '_'
  done;
  s'

let extract_tax_info decor =
  match List.filter (function | Decor.Taxinfo _ -> true | _ -> false) decor with
  | [Decor.Taxinfo (tid,n)] -> (tid,n)
  | _ -> assert(false)

(* make a map from a list of edge ids to the most likely vectors on either side
 * of the edge: order is (distal, proximal) *)
let pickpair_map summarize_f initial model t ~darr ~parr ids =
  let u1 = Glv.mimic (Glv_arr.get_one darr)
  and u2 = Glv.mimic (Glv_arr.get_one darr) in
  List.fold_right
    (fun id ->
      IntMap.add
        id
        (Seq_post.get_summary Seq_post.Distal summarize_f initial u1 u2 model t ~darr ~parr id,
         Seq_post.get_summary Seq_post.Proximal summarize_f initial u1 u2 model t ~darr ~parr id))
    ids
    IntMap.empty

let write_map_info ~darr ~parr rp =
  let t = Refpkg.get_tax_ref_tree rp
  and name = Refpkg.get_name rp
  and model = Refpkg.get_model rp
  and mrcal = Refpkg.get_mrcam rp |> IntMap.keylist
  and code = Model.code (Refpkg.get_model rp) in
  let tax_info_of_id id = extract_tax_info (Gtree.get_bark t id)#get_decor in
  let taxid_of_id id = match fst (tax_info_of_id id) with
    | Tax_id.TaxStr s -> s
    | Tax_id.NoTax -> assert(false)
  and name_of_id id = snd (tax_info_of_id id)
  in
  let distal_str_map =
    IntMap.map
      (fun (at_d, _) -> (Model.to_sym_str code at_d))
      (pickpair_map Gsl_vector.max_index (-1) model t ~darr ~parr mrcal)
  in
  let ch_picks = open_out (name^".picks") in
  let max_rat v = (Gsl_vector.max v) /. (Linear_utils.l1_norm v) in
  let ch_likes = open_out (name^".likes") in
  IntMap.iter
    (fun id (at_d, at_p) ->
      Printf.fprintf ch_likes "%s\t" (taxid_of_id id);
      Printf.fprintf ch_picks ">%s\n" (name_of_id id);
      let distal_str = IntMap.find id distal_str_map in
      for i=0 to (Array.length at_d) - 1 do
        let d = at_d.(i) and _ = at_p.(i) in
        Printf.fprintf ch_likes "%g\t" d;
        if d > 0.8 then begin
          output_char ch_picks distal_str.[i];
        end
        else output_char ch_picks '-';
      done;
      output_char ch_picks '\n';
      output_char ch_likes '\n';
      )
    (pickpair_map max_rat 0. model t ~darr ~parr mrcal);
  close_out ch_likes;
  close_out ch_picks;

  ()
