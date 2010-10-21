(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
   For each x we consider the lineage of x, and Fold_left over the taxtree: fun
     (x, anc) -> ListFuns.add_listly (anc, x)
  keep track of the ones without ancestors.
    make sure that there is only one at the end, and keep it.
      Then we List.rev the maps.
*)

open Tax_id

let add_list = List.fold_right TaxIdSet.add 

(* get the most distal taxa in the taxonomy which are represented in til. 
 * preserve the order of til as much as possible *)
let tax_tips_of_tax_list td til = 
  (* s is the set of things we have seen, accu is our list of tips *)
  let rec aux s accu = function
    | x::l -> 
        if TaxIdSet.mem x s then aux s accu l
        else begin
          let lin = Tax_taxonomy.get_lineage td x in
          let removes = TaxIdSetFuns.of_list lin in
          (* below: we add the lineage of our taxonomy
           * and take our ancestors from accu if present *)
          aux
            (add_list lin s) 
            (x::(List.filter (fun x -> not (TaxIdSet.mem x removes)) accu))
            l
        end
    | [] -> accu
  in
  List.rev (aux TaxIdSet.empty [] til)



