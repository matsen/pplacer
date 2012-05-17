(* bark for trees that have newick + a list of Decor decorations.
*)

open Ppatteries


class decor_bark arg =
  let (bl, node_label, edge_label, decor) =
    match arg with
    | `Empty -> (None, None, None, [])
    | `Of_newick_bark nb ->
        (nb#get_bl_opt, nb#get_node_label_opt, nb#get_edge_label_opt, [])
    | `Of_bl_node_edge_label_decor (bl, node_label, edge_label, decor) ->
        (bl, node_label, edge_label, decor)
  in
  object (* (self) *)
    val decor = decor
    inherit Newick_bark.newick_bark
      (`Of_bl_node_edge_label (bl, node_label, edge_label))
      as super

    method get_decor = decor

    method ppr ff =
      Format.fprintf ff "@[{%a decor = %a;}@]"
        (fun ff () -> super#ppr_inners ff) ()
        (Ppr.ppr_list Decor.ppr) decor

    method to_xml is_leaf =
      super#to_xml is_leaf
      @ List.flatten (List.map Decor.to_xml (List.sort compare decor))

    method set_decor decor_list =
      {< decor = decor_list >}

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
    (`Of_bl_node_edge_label
        (db#get_bl_opt, db#get_node_label_opt, db#get_edge_label_opt))
