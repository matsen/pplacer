open Ppatteries
open Subcommand
open Guppy_cmdobjs

let only_one a b =
  (a && not b) || (b && not a)

let unifrac_uptri founds bl =
  Uptri.init
    (Array.length founds)
    (fun i j ->
      (if only_one founds.(i) founds.(j) then bl else 0.),
      (if founds.(i) || founds.(j) then bl else 0.))

let pair_sum ut1 ut2 =
  Uptri.apply_pairwise (fun (a, b) (c, d) -> a +. c, b +. d) ut1 ut2

let update_found founds masses =
  Array.iteri
    (fun i m -> if approx_compare m 0. > 0 then founds.(i) <- true)
    masses

let unifrac ref_tree ml =
  let open Kr_distance in
  let starter_vec = Array.make (List.length ml) false in
  let starter_uptri = Uptri.create (Array.length starter_vec) (0., 0.)
  and kr_map = make_n_kr_map ml in
  let kr_edge_total id =
    general_total_along_edge
      ~merge_r:pair_sum
      unifrac_uptri
      (Gtree.get_bl ref_tree id)
      (IntMap.get id [] kr_map)
      update_found
  and merge_founds fl =
    let ret = Array.copy starter_vec in
    List.iter
      (Array.iteri (fun i v -> if v then ret.(i) <- true))
      fl;
    ret
  in
  general_total_over_tree
    kr_edge_total
    (const ())
    ~r_list_sum:(List.fold_left pair_sum starter_uptri)
    merge_founds
    ~starter_r_factory:(const starter_uptri)
    (fun () -> Array.copy starter_vec)
    (Gtree.get_stree ref_tree)
  |> Uptri.map (uncurry (/.))

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit mass_cmd () as super_mass
  inherit tabular_cmd () as super_tabular

  val list_output = flag "--list-out"
    (Plain (false, "Output the unifrac results as a list rather than a matrix."))

  method specl =
    super_mass#specl
  @ super_tabular#specl
  @ [toggle_flag list_output]

  method desc =
"calculates unifrac on two or more placefiles"
  method usage = "usage: unifrac 1.jplace 2.jplace [3.jplace [...]]"

  method private nontrivial_placefile_action prl =
    let gt = Mokaphy_common.list_get_same_tree prl
      |> Newick_gtree.add_zero_root_bl
    and weighting, criterion = self#mass_opts in
    let ml = List.map (Mass_map.Indiv.of_placerun weighting criterion) prl
    and names = List.map Placerun.get_name prl |> Array.of_list in
    let uptri = unifrac gt ml in
    if fv list_output then begin
      let res = RefList.empty () in
      RefList.push res ["sample_1"; "sample_2"; "unifrac"];
      Uptri.iterij
        (fun i j x ->
          RefList.push res [names.(i); names.(j); Printf.sprintf "%g" x])
        uptri;
      RefList.to_list res |> List.rev |> self#write_ll_tab
    end else
      Mokaphy_common.write_named_float_uptri self#out_channel names uptri

  method private placefile_action prl =
    match List.length prl with
      | (0 | 1) as n ->
        Printf.sprintf "unifrac requires two or more placefiles (%d given)" n
        |> failwith

      | _ -> self#nontrivial_placefile_action prl

end
