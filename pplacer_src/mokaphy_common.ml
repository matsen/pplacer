
exception Refpkg_tree_and_ref_tree_mismatch
exception Uptri_dim_mismatch

let spec_with_default symbol setfun p help =
  (symbol, setfun p, Printf.sprintf help !p)

let weighted_help =
  "The point version simply uses the best placement, rather than spreading out the probability mass. Default is spread."

let refpkg_help fors =
  "Specify a reference package to use the taxonomic version of "^fors^"."

let transform_help =
    "A transform to apply to the read multiplicities before calculating. \
    Options are 'log' and 'unit'. Default is no transform."

let chop_suffix_if_present s suff =
  if Filename.check_suffix s suff then Filename.chop_suffix s suff
  else s

(* make sure all the trees in the placerun list are the same *)
let list_get_same_tree = function
  | [] -> assert(false)
  | [x] -> Placerun.get_ref_tree x
  | hd::tl -> List.hd (List.map (Placerun.get_same_tree hd) tl)

let cat_names prl =
  String.concat "." (List.map Placerun.get_name prl)

(* *** making pres *** *)
let prel_of_prl weighting criterion prl =
  List.map (Mass_map.Pre.of_placerun weighting criterion) prl

let make_tax_pre taxt weighting criterion ti_imap pr =
  Tax_mass.pre
    (Gtree.top_id taxt)
    Placement.contain_classif
    weighting
    criterion
    ti_imap
    pr

(* *** refpkgs *** *)
let refpkgo_of_fname = function
  | "" -> None
  | path -> Some (Refpkg.of_path path)

let check_refpkgo_tree ref_tree = function
  | None -> ()
  | Some rp ->
      if 0 <> compare
                (Gtree.get_stree ref_tree)
                (Gtree.get_stree (Refpkg.get_ref_tree rp))
      then
        raise Refpkg_tree_and_ref_tree_mismatch

(* *** output tools *** *)
(* there is a lack of parallelism here, as write_unary takes placeruns, while
 * uptri takes an uptri, but uptri needs to be more general. *)

let write_unary pr_to_float prl ch =
  String_matrix.write_padded
   ch
   (Array.map
     (fun pr ->
       [|
         Placerun.get_name pr;
         Printf.sprintf "%g" (pr_to_float pr);
       |])
   (Array.of_list prl))

let write_uptri list_output namea fun_name u ch =
  if Uptri.get_dim u = 0 then
    failwith(Printf.sprintf "can't do %s with fewer than two place files" fun_name);
  if list_output then begin
    String_matrix.write_padded ch
      (Array.of_list
        ([|"sample_1"; "sample_2"; fun_name;|]::
          (let m = ref [] in
          Uptri.iterij
            (fun i j s -> m := [|namea.(i); namea.(j); s|]::!m)
            (Uptri.map (Printf.sprintf "%g") u);
          List.rev !m)))
  end
  else begin
    Printf.fprintf ch "%s distances:\n" fun_name;
    Mokaphy_base.write_named_float_uptri ch namea u;
  end


let write_uptril list_output namea fun_namel ul ch =
  match ul with
  | [] -> ()
  | hd::tl ->
  if 0 =
    List.fold_left
      (fun d u -> if d = Uptri.get_dim u then d else raise Uptri_dim_mismatch)
      (Uptri.get_dim hd)
      tl
  then
    failwith "can't do anything interesting with fewer than two place files";
  if list_output then begin
    let make_line i j =
      Array.of_list
        ([namea.(i); namea.(j)] @
          (List.map (fun u -> Printf.sprintf "%g" (Uptri.get u i j)) ul))
    in
    String_matrix.write_padded ch
      (Array.of_list
        ((Array.of_list (["sample_1";"sample_2"] @ fun_namel))::
          (let m = ref [] in
          Uptri.iterij (fun i j _ -> m := ((make_line i j)::!m)) hd;
          List.rev !m)))
  end
  else begin
    List.iter2
      (fun fun_name u ->
        Printf.fprintf ch "%s distances:\n" fun_name;
        Mokaphy_base.write_named_float_uptri ch namea u;)
      fun_namel ul
  end
