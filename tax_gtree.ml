(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for gtrees which have a tax_bark map.
*)

open Fam_batteries
open MapsSets

(*
let compare t1 t2 = Gtree.compare Decor_bark.compare t1 t2
  Gtree.mapi_bark_map (fun _ b -> Tax_bark.of_newick_bark b) t
*)

let annotate_newick t td = 
  let tips_annotated = 
    IntMap.map
      (fun newick_bark ->
        Tax_bark.of_newick_bark
          newick_bark
          (match newick_bark#get_name_opt with
          | Some name -> Some (Tax_refdata.tax_id_by_name td name)
          | None -> None))
      (Gtree.get_bark_map t)
  in
  Gtree.set_bark_map t tips_annotated



let f = Tax_data.get_rank_name
let f = Newick.of_file
