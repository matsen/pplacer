(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 * The fact that kr_core is in its own file is simply because it's big.
 *)

open MapsSets
open Fam_batteries




(* *** AVGDIST AVGDIST AVGDIST AVGDIST AVGDIST  *** *)

let make_dist_fun prefs prl = 
  Pquery_distances.dist_fun_of_expon_weight 
    (Mokaphy_prefs.Avgdst.exponent prefs)
    (Mokaphy_prefs.weighting_of_bool (Mokaphy_prefs.Avgdst.weighted prefs))
    (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.Avgdst.use_pp prefs))
    (Edge_rdist.build_ca_info (Cmds_common.list_get_same_tree prl))

let uavgdst prefs prl = 
  Cmds_common.wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (Cmds_common.write_unary
      (Avgdst.of_placerun 
        (make_dist_fun prefs prl))
      prl)

let bavgdst prefs prl = 
  let pra = Array.of_list prl in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.Avgdst.out_fname prefs) 
    (Cmds_common.write_uptri
      (Mokaphy_prefs.Avgdst.list_output prefs)
      (Array.map Placerun.get_name pra)
      "bavgdst"
      (Uptri.init
        (Array.length pra)
        (fun i j -> 
          Avgdst.of_placerun_pair 
            (make_dist_fun prefs prl)
            pra.(i) 
            pra.(j))))



(* *** BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ *** *)
let bootviz prefs = function
  | [ct_fname] ->
      let out_fname = match Mokaphy_prefs.Bootviz.out_fname prefs with
        | "" -> "cluster_boot.xml" 
        | s -> s
      in
      Phyloxml.tree_to_file
        (Bootviz.decorate_tree 
          (Mokaphy_prefs.Bootviz.cutoff prefs) 
          (Mokaphy_prefs.Bootviz.boot_fname prefs)
          ct_fname)
        out_fname
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootviz."


(* *** BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB *** *)
let bootsub prefs = function
  | [ct_fname] -> begin
      match (Mokaphy_prefs.Bootsub.boot_fname prefs,
            Mokaphy_prefs.Bootsub.name_csv prefs) with
        | "",_ -> failwith "please supply a file of bootstrapped trees"
        | _,"" -> failwith "please supply a cluster CSV file"
        | (boot_fname, csv_fname) -> 
            Cmds_common.wrap_output (Mokaphy_prefs.Bootsub.out_fname prefs)
              (fun ch ->
                Bootsub.perform 
                  ch
                  (Mokaphy_prefs.Bootsub.cutoff prefs) 
                  ~csv_fname
                  ~boot_fname
                  ~ct_fname)
      end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootsub."


(* *** PCA PCA PCA PCA PCA *** *)
let pca prefs = function
  | [] -> ()
  | prl ->
      match Mokaphy_prefs.Pca.out_prefix prefs with 
      | "" -> failwith "Please specify an out prefix for pca with -o"
      | prefix ->
      Pca.pca_complete 
        ~scale:(Mokaphy_prefs.Pca.scale prefs)
        (Mass_map.transform_of_str (Mokaphy_prefs.Pca.transform prefs))
        (Mokaphy_prefs.weighting_of_bool (Mokaphy_prefs.Pca.weighted prefs))
        (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.Pca.use_pp prefs))
        (Mokaphy_prefs.Pca.multiplier prefs)
        (Mokaphy_prefs.Pca.write_n prefs)
        (Cmds_common.refpkgo_of_fname (Mokaphy_prefs.Pca.refpkg_path prefs))
        prefix
        prl
