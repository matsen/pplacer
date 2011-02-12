
module Prefs = struct
  type mokaphy_prefs = 
    {
      use_pp: bool ref;
      out_fname: string ref;
      normalized: bool ref;
    }
  
  let use_pp            p = !(p.use_pp)
  let out_fname         p = !(p.out_fname)
  let normalized        p = !(p.normalized)
  
  let defaults () = 
    { 
      use_pp = ref false;
      out_fname = ref "";
      normalized = ref false;
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Set the filename to write to. Otherwise write to stdout.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "--normalized", Arg.Set prefs.normalized,
    "Divide by total tree length.";
    ]
end 


let pd prefs prl = 
  let pd_cmd = 
    if Prefs.normalized prefs then Pd.normalized_of_pr
    else Pd.of_pr
  in
  Cmds_common.wrap_output 
    (Prefs.out_fname prefs) 
    (Cmds_common.write_unary 
      (pd_cmd (Mokaphy_prefs.criterion_of_bool (Prefs.use_pp prefs)))
      prl)


