(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 *)

exception Refpkg_tree_and_ref_tree_mismatch


(* for out_fname options *)
let ch_of_fname = function
  | "" -> stdout
  | s -> open_out s

let wrap_output fname f = 
  let ch = ch_of_fname fname in
  f ch;
  if ch <> stdout then close_out ch

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
let pre_of_pr ~is_weighted ~use_pp pr = 
  Mass_map.Pre.of_placerun 
    (Mokaphy_prefs.weighting_of_bool is_weighted)
    (Mokaphy_prefs.criterion_of_bool use_pp)
    pr

let prel_of_prl ~is_weighted ~use_pp prl = 
  List.map (pre_of_pr ~is_weighted ~use_pp) prl

let make_tax_pre ~is_weighted ~use_pp ti_imap pr =
  Tax_mass.of_placerun 
    Placement.contain_classif 
    (Mokaphy_prefs.weighting_of_bool is_weighted)
    (Mokaphy_prefs.criterion_of_bool use_pp) 
    ti_imap 
    pr

(* *** refpkgs *** *)
let refpkgo_of_fname = function
  | "" -> None
  | path -> Some (Refpkg.of_path path)

let check_refpkgo_tree ref_tree = function
  | None -> ()
  | Some rp -> 
      if 0 <> Newick.compare ref_tree (Refpkg.get_ref_tree rp) then
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

let write_uptri fun_name list_output namea u ch = 
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

