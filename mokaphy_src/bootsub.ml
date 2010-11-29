(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

module StrBootsub = 
  Bootsubfunc.Make 
    (MapsSets.OrderedString) 
    (MapsSets.StringSet) 
    (Cluster_common.StringSetSet)

let perform ch cutoff ~csv_fname ~boot_fname ~ct_fname = 
  let ct = Newick.of_file ct_fname
  and boot_tl = Newick.list_of_file boot_fname
  in
  let boot_sssl = List.map Cluster_common.sss_of_tree boot_tl
  and taxon_list t = List.map (Gtree.get_name t) (Gtree.leaf_ids t) 
  in
  let taxon_set t = List.fold_right StringSet.add (taxon_list t) StringSet.empty
  in
  let taxs = taxon_set ct
  and ssim = Cluster_common.ssim_of_tree ct
  in
  List.iter 
    (fun ss -> if 0 <> StringSet.compare taxs ss then 
                 invalid_arg "taxon sets not identical")
    (List.map taxon_set boot_tl);
  List.iter
    (fun (id, name) ->
      Csv.save_out ch
        ([]::[name]::
          (List.map
            (fun (score, stro) ->
              [string_of_float score;
              match stro with None -> "" | Some s -> s])
            (StrBootsub.perform cutoff (IntMap.find id ssim) boot_sssl))))
    (Cluster_common.numnamel_of_csv csv_fname)
