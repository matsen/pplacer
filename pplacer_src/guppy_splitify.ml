open Subcommand
open Guppy_cmdobjs

open Ppatteries

(* *** splitify *** *)

let fal_to_strll fal =
  List.map
    (fun (name, v) -> name::(List.map string_of_float (Array.to_list v)))
    fal

let save_out_named_fal ch fal =
  csv_out_channel ch
    |> Csv.to_out_obj
    |> flip Csv.output_all (fal_to_strll fal)

let find_rep_edges max_edge_d fal gt =
  let dist i j =
    List.map (fun arr -> (arr.(i) -. arr.(j)) ** 2.) fal
    |> List.fsum
    |> sqrt
  in
  let open Stree in
  let rec aux = function
    | Leaf i -> IntSet.empty, IntSet.singleton i
    | Node (i, subtrees) ->
      let rep_edges, possible_cur_edges = List.fold_left
        (fun (rea, pcea) t ->
          let re, pce = aux t in IntSet.union re rea, IntSet.union pce pcea)
        (IntSet.empty, IntSet.empty)
        subtrees
      in
      let cur_edges, far_edges = IntSet.partition
        (fun j -> dist i j < max_edge_d)
        possible_cur_edges
      in
      IntSet.union rep_edges far_edges,
      if IntSet.is_empty cur_edges then IntSet.singleton i else cur_edges
  in
  Gtree.get_stree gt |> aux |> uncurry IntSet.union

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit mass_cmd () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit splitify_cmd () as super_splitify

  val rep_edges = flag "--rep-edges"
    (Needs_argument ("", "rep edges"))

  method specl =
    float_flag rep_edges
 :: super_output#specl
  @ super_mass#specl
  @ super_splitify#specl

  method desc =
"writes out differences of masses for the splits of the tree"
  method usage = "usage: splitify [options] placefile(s)"

  method private placefile_action prl =
    let weighting, criterion = self#mass_opts in
    let data = List.map (self#splitify_placerun weighting criterion) prl
    and names = (List.map Placerun.get_name prl) in

    let data = match fvo rep_edges with
      | None -> data
      | Some max_edge_d ->
        let edges = Mokaphy_common.list_get_same_tree prl
          |> find_rep_edges max_edge_d data
        in
        List.map (Array.filteri (fun i _ -> IntSet.mem i edges)) data
    in

    save_out_named_fal
      self#out_channel
      (List.combine names data)

end
