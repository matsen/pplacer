open Ppatteries
open Subcommand
open Guppy_cmdobjs
open Placerun


class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  method desc =
"writes the number of leaves of the reference tree and the number of pqueries"
  method usage = "usage: info [options] placefile[s]"

  method private placefile_action prl =
    let mat = Array.make_matrix ((List.length prl) + 1) 4 "" in
    mat.(0) <- [|"name"; "leaves"; "pqueries"; "multi_pqueries"|];
    List.iteri
      (fun e pr ->
        mat.(e + 1) <- [|
          pr.name;
          string_of_int (Gtree.n_taxa pr.ref_tree);
          string_of_int (n_pqueries pr);
          string_of_int
            (List.fold_left
               (fun accum pq -> accum + (List.length (Pquery.namel pq)))
               0
               pr.pqueries);
        |])
      prl;

    self#write_aa_tab mat

end
