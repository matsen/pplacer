open MapsSets
open Fam_batteries


module Prefs = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      out_fname: string ref;
      list_output: bool ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let out_fname         p = !(p.out_fname)
  let list_output       p = !(p.list_output)
  
  let defaults () = 
    { 
      use_pp = ref false;
      out_fname = ref "";
      list_output = ref false
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--list-out", Arg.Set prefs.list_output,
    "Output the UniFrac results as a list rather than a matrix.";
    ]
end 

let of_induceds t ind1 ind2 = 
  let isect = Induced.intersect t ind1 ind2
  and union = Induced.union t ind1 ind2
  in
  (* to play it safe for a while *)
  List.iter (Induced.check t) [ind1; ind2; isect; union;];
  (Pd.of_induced t isect) /. (Pd.of_induced t union)

let unifrac prefs prl = 
  let t = Cmds_common.list_get_same_tree prl
  and pra = Array.of_list prl
  in
  let inda = 
    Array.map
      (Induced.of_placerun 
        (Mokaphy_prefs.criterion_of_bool (Prefs.use_pp prefs)))
      pra
  in
  Cmds_common.wrap_output 
    (Prefs.out_fname prefs) 
    (Cmds_common.write_uptri
      (Prefs.list_output prefs)
      (Array.map Placerun.get_name pra)
      "unifrac"
      (Uptri.init
        (Array.length inda)
        (fun i j -> of_induceds t inda.(i) inda.(j))))
