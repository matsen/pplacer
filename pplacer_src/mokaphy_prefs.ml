(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * This should have no nontrivial dependencies, so that it can be used anywhere.
 *)

let spec_with_default symbol setfun p help = 
  (symbol, setfun p, Printf.sprintf help !p)

let weighted_help = 
  "The point version simply uses the best placement, rather than spreading out the probability mass. Default is spread."

let refpkg_help fors = 
  "Specify a reference package to use the taxonomic version of "^fors^"."

let transform_help =
    "A transform to apply to the read multiplicities before calculating. \
    Options are 'log' and 'unit'. Default is no transform."

(* the following two options are common between many prefs *)

(* weighted option *)
let weighting_of_bool = function
  | true -> Mass_map.Weighted 
  | false -> Mass_map.Unweighted 

(* use_pp option *)
let criterion_of_bool = function
  | true -> Placement.post_prob
  | false -> Placement.ml_ratio




(* BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ BOOTVIZ *)
module Bootviz = struct
  type mokaphy_prefs = 
    {
      boot_fname: string ref;
      out_fname: string ref;
      cutoff: float ref;
    }
  
  let boot_fname p = !(p.boot_fname)
  let out_fname p = !(p.out_fname)
  let cutoff p = !(p.cutoff)
  
  let defaults () = 
    { 
      boot_fname = ref "";
      out_fname = ref "";
      cutoff = ref 0.;
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-b", Arg.Set_string prefs.boot_fname,
    "The file containing the bootstrapped trees, one per line.";
    "-o", Arg.Set_string prefs.out_fname,
    "Specify an out file (default cluster_boot.xml).";
    "--cutoff", Arg.Set_float prefs.cutoff,
    "Specify the cutoff for writing out the bootstrap value (default 0).";
    ]
end 


(* BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB BOOTSUB *)
module Bootsub = struct
  type mokaphy_prefs = 
    {
      boot_fname: string ref;
      out_fname: string ref;
      cutoff: float ref;
      name_csv: string ref;
    }
  
  let boot_fname p = !(p.boot_fname)
  let out_fname p = !(p.out_fname)
  let cutoff p = !(p.cutoff)
  let name_csv p = !(p.name_csv)
  
  let defaults () = 
    { 
      boot_fname = ref "";
      name_csv = ref "";
      out_fname = ref "";
      cutoff = ref 0.95;
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-b", Arg.Set_string prefs.boot_fname,
    "The file containing the bootstrapped trees, one per line.";
    "--name-csv", Arg.Set_string prefs.name_csv,
    "A CSV file containing two columns: \"numbers\", which are node numbers in the clustered tree, and \"names\", which are names for those nodes.";
    "-o", Arg.Set_string prefs.out_fname,
    "Specify an out filename.";
    "--cutoff", Arg.Set_float prefs.cutoff,
    "Specify the cutoff for writing out the bootstrap value.";
    ]
end 


(* PCA PCA PCA PCA PCA PCA PCA PCA *)
module Pca = struct
  type mokaphy_prefs = 
    {
      out_prefix: string ref;
      use_pp: bool ref;
      weighted: bool ref;
      write_n: int ref;
      refpkg_path : string ref;
      scale: bool ref;
      multiplier: float ref;
      transform: string ref;
    }
  
  let out_prefix  p = !(p.out_prefix)
  let use_pp      p = !(p.use_pp)
  let weighted    p = !(p.weighted)
  let write_n     p = !(p.write_n)
  let refpkg_path p = !(p.refpkg_path)
  let scale       p = !(p.scale)
  let multiplier  p = !(p.multiplier)
  let transform   p = !(p.transform)
  
  let defaults () = 
    { 
      out_prefix = ref "";
      use_pp = ref false;
      weighted = ref false;
      write_n = ref 5;
      refpkg_path = ref "";
      scale = ref false;
      multiplier = ref 50.;
      transform = ref "";
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_prefix,
    "Specify an out prefix.";
    "-p", Arg.Set prefs.use_pp,
    "Use posterior probability.";
    "-c", Arg.Set_string prefs.refpkg_path,
    (refpkg_help "cluster");
    "--unweighted", Arg.Clear prefs.weighted,
    weighted_help;
    "--write-n", Arg.Set_int prefs.write_n,
    "The number of principal coordinates to write out (default is 5).";
    "--scale", Arg.Set prefs.scale,
    "Scale variances to one before performing principal components.";
    "--multiplier", Arg.Set_float prefs.multiplier,
    "The factor by which we multiply the principal component eigenvectors to get branch thickness.";
    "--transform", Arg.Set_string prefs.transform,
    transform_help;
    ]
end 
