open Subcommand
open Guppy_cmdobjs
open MapsSets
open Fam_batteries

let named_arr_list_of_csv fname =
  List.map
    (function
      | [] -> assert(false)
      | name::entries ->
          (name, Array.of_list (List.map float_of_string entries)))
    (Csv.load fname)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit heat_cmd () as super_heat
  inherit refpkg_cmd ~required:true as super_refpkg

  method specl =
    super_output#specl
    @ super_refpkg#specl
    @ super_heat#specl

  method desc =
"maps an an arbitrary vector of the correct length to the tree"
  method usage = "usage: heat -o my.xml -c my.refpkg matrix.csv"

  method private csv_to_named_trees t fname =
    (* Below: add one for the root edge. *)
    let n_edges = 1+Gtree.n_edges t in
    List.map
      (fun (name, a) ->
        if n_edges <> Array.length a then
          failwith
            (Printf.sprintf
              "%d entries in %s, and %d edges in reference tree"
              (Array.length a) name n_edges);
        (Some name, self#heat_tree_of_float_arr t a))
      (named_arr_list_of_csv fname)

  method action = function
    | [] -> ()
    | pathl ->
        let t = self#get_decor_ref_tree in
        List.iter
          (fun fname ->
            Phyloxml.pxdata_to_channel self#out_channel
              (Phyloxml.pxdata_of_named_gtrees
                (self#csv_to_named_trees t fname)))
          pathl
end
