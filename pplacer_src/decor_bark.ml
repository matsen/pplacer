(* bark for trees that have newick + a list of Decor decorations.
*)

open Ppatteries


class decor_bark arg =
  let (bl, name, boot, decor) =
    match arg with
    | `Empty -> (None, None, None, [])
    | `Of_newick_bark nb ->
        (nb#get_bl_opt, nb#get_name_opt, nb#get_boot_opt, [])
    | `Of_bl_name_boot_decor (bl, name, boot, decor) ->
        (bl, name, boot, decor)
  in
  object (* (self) *)
    val decor = decor
    inherit Newick_bark.newick_bark
      (`Of_bl_name_boot (bl, name, boot))
      as super

    method get_decor = decor

    method ppr ff =
      Format.fprintf ff "@[{%a decor = %a;}@]"
        (fun ff () -> super#ppr_inners ff) ()
        (Ppr.ppr_list Decor.ppr) decor

    method to_xml =
      super#to_xml
      @ List.flatten (List.map Decor.to_xml (List.sort decor))

    method append_decor decor_list =
      {< decor = decor @ decor_list >}

  end

let compare b1 b2 =
  try
    raise_if_different Newick_bark.compare b1 b2;
    raise_if_different compare b1#get_decor b2#get_decor;
    0
  with
  | Different c -> c

let of_newick_bark nb = new decor_bark (`Of_newick_bark nb)
let to_newick_bark db =
  new Newick_bark.newick_bark
        (`Of_bl_name_boot (db#get_bl_opt, db#get_name_opt, db#get_boot_opt))
