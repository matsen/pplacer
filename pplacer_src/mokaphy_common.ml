open Ppatteries

let write_named_float_uptri ch names u =
  String_matrix.write_named_padded ch names
    (MatrixFuns.init (Uptri.get_dim u) (Uptri.get_dim u)
      (fun i j ->
        if i < j then (Printf.sprintf "%g" (Uptri.get u i j))
        else ""))

(* make sure all the trees in the placerun list are the same *)
let list_get_same_tree = function
  | [] -> invalid_arg "list_get_same_tree"
  | [x] -> Placerun.get_ref_tree x
  | hd::tl -> List.hd (List.map (Placerun.get_same_tree hd) tl)

let cat_names prl =
  String.concat "." (List.map Placerun.get_name prl)

(* *** making pres *** *)
let make_tax_pre taxt weighting criterion ti_imap pr =
  Tax_mass.pre
    (Gtree.top_id taxt)
    Placement.classif
    weighting
    criterion
    ti_imap
    pr
