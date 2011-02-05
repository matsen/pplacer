(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *)

module TIAMR = AlgMap.AlgMapR (Tax_id.OrderedTaxId)

(* if rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank *)
let classify_at_rank td rank ti = 
  let rec aux curr_ti = 
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then curr_ti
    else 
      aux 
        (try Tax_taxonomy.get_ancestor td curr_ti with
        | Tax_taxonomy.NoAncestor _ -> assert(false))
  in
  aux ti

(* apply f to all of the keys and add the results together *)
let keymap_add_by f m = 
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (Tax_id.TaxIdMapFuns.to_pairs m)
    TIAMR.M.empty

(* m is a taxid_algmap and this outputs a list of string_arrays, one for each
 * placement *)
let classif_stral td name desired_rank m =
  List.map
    (fun (ti, p) ->
      [|
        name;
        Tax_taxonomy.get_rank_name td desired_rank;
        Tax_taxonomy.rank_name_of_tax_id td ti;
        Tax_id.to_bare_str ti;
        Printf.sprintf "%g" p;
      |])
    (Tax_id.TaxIdMapFuns.to_pairs m)

let classify how criterion rp prl =
  let td = Refpkg.get_taxonomy rp in
  let n_ranks = Tax_taxonomy.get_n_ranks td in
  List.iter 
    (fun pr ->
      let ch = open_out ((Placerun.get_name pr)^".sqlclassif") in
      try 
        List.iter 
          (fun pq ->
            let outl = ref [] in
            let m = ref 
              (List.fold_right
                (fun p ->
                  TIAMR.add_by
                    (how p)
                    (criterion p))
                (Pquery.place_list pq)
                (TIAMR.M.empty))
            in
            for desired_rank=(n_ranks-1) downto 0 do
              m := keymap_add_by (classify_at_rank td desired_rank) !m;
              outl := 
                (List.flatten 
                  (List.map
                    (fun name -> classif_stral td name desired_rank !m)
                    (Pquery.namel pq)))
                @ (!outl);
            done;
            String_matrix.write_padded ch (Array.of_list (!outl));)
          (Placerun.get_pqueries pr);
          close_out ch;
      with
      | Placement.No_classif ->
          invalid_arg 
            ((Placerun.get_name pr)^" contains unclassified queries!"))
    prl
