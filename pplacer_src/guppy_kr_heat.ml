open Subcommand
open Guppy_cmdobjs
open Ppatteries

(* Make a map with the amount of transport along each edge. *)
let transport_map t pre1 pre2 =
  let kr_map =
    IntMap.map
    (* we don't care about where we are along the edge *)
      (List.map snd)
      (Kr_distance.make_n_kr_map
        [Mass_map.Indiv.of_pre pre1;
         Mass_map.Indiv.of_pre pre2]) in
  let sum_over_krs_of_id id =
    List.fold_right
      (fun kr_v -> ( +. ) (kr_v.(0) -. kr_v.(1)))
      (IntMap.get id [] kr_map)
  in
  let heat_list =
    Stree.recur_listly
      (fun id below ->
        ((id,
          sum_over_krs_of_id
            id
(* the first item a list from a subtree is the total of all of the heat in that
* subtree. therefore to get the total heat for our tree, we just have to total
* all of those *)
            (List.fold_left
              (fun accu -> function
                | (_,heat)::_ -> heat +. accu
                | [] -> accu)
              0.
              below)))
        :: (List.flatten below))
      (Gtree.get_stree t)
  in
  let (_,top_heat) = List.hd heat_list in
  if top_heat > Kr_distance.tol then
    raise (Kr_distance.Total_kr_not_zero top_heat);
  IntMap.of_pairlist heat_list

(* The commands *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd () as super_mass
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit kr_cmd () as super_kr
  inherit heat_cmd () as super_heat
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  method specl =
    super_mass#specl
    @ super_refpkg#specl
    @ super_kr#specl
    @ super_heat#specl
    @ super_output#specl

  method desc =
"makes a heat tree"
  method usage = "usage: heat [options] placefile1 placefile2"

  method private placefile_action = function
    | [pr1; pr2] as prl ->
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".heat.xml"))
        ()
      in
      let weighting, criterion = self#mass_opts
      and tree_name = Mokaphy_common.chop_suffix_if_present fname ".xml" in
      let my_pre_of_pr = Mass_map.Pre.of_placerun weighting criterion
      and refpkgo, ref_tree = self#get_rpo_and_tree pr1 in
      let make_heat_tree decor_t pre1 pre2 =
        self#spread_short_fat
          (Decor_gtree.add_decor_by_map
            decor_t
            (self#decor_map_of_float_map
              (transport_map decor_t pre1 pre2)))
      in
      Phyloxml.named_gtrees_to_file
        fname
        ([Some tree_name,
          make_heat_tree
            (self#maybe_numbered ref_tree)
            (my_pre_of_pr pr1)
            (my_pre_of_pr pr2)]
        @ match refpkgo with
        | None -> []
        | Some rp -> begin
            let (taxt, ti_imap) = Tax_gtree.of_refpkg_unit rp in
            let my_make_tax_pre =
              Mokaphy_common.make_tax_pre taxt weighting criterion ti_imap in
            [Some (tree_name^".tax"),
            make_heat_tree taxt
              (my_make_tax_pre pr1)
              (my_make_tax_pre pr2)]
        end)

    | l ->
      List.length l
      |> Printf.sprintf "kr_heat takes exactly two placefiles (%d given)"
      |> failwith

end
