
module Prefs = struct
  type mokaphy_prefs =
    {
      boot_fname: string ref;
      out_fname: string ref;
      cutoff: float ref;
    }

  let boot_fname p = !(p.boot_fname)
  let out_fname p = !(p.out_fname)
  let cutoff p = !(p.cutoff)

  let defaults () =
    {
      boot_fname = ref "";
      out_fname = ref "";
      cutoff = ref 0.;
    }

  (* arguments *)
  let specl_of_prefs prefs = [
    "-b", Arg.Set_string prefs.boot_fname,
    "The file containing the bootstrapped trees, one per line.";
    "-o", Arg.Set_string prefs.out_fname,
    "Specify an out file (default cluster_boot.xml).";
    "--cutoff", Arg.Set_float prefs.cutoff,
    "Specify the cutoff for writing out the bootstrap value (default 0).";
    ]
end


let bootviz prefs = function
  | [ct_fname] ->
      let out_fname = match Prefs.out_fname prefs with
        | "" -> "cluster_boot.xml"
        | s -> s
      in
      Phyloxml.gtree_to_file
        out_fname
        (Bootviz.decorate_tree
          (Prefs.cutoff prefs)
          (Prefs.boot_fname prefs)
          ct_fname)
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootviz."


