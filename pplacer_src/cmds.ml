(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 * The fact that kr_core is in its own file is simply because it's big.
 *)

open MapsSets
open Fam_batteries

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
