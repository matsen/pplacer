(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Applying preferences and running commands.
 *
 * The fact that kr_core is in its own file is simply because it's big.
 *)

open MapsSets
open Fam_batteries



(* *** PD PD PD PD PD *** *)
let pd prefs prl = 
  let pd_cmd = 
    if Mokaphy_prefs.PD.normalized prefs then Pd.normalized_of_pr
    else Pd.of_pr
  in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.PD.out_fname prefs) 
    (Cmds_common.write_unary 
      (pd_cmd (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PD.use_pp prefs)))
      prl)


(* *** PDFRAC PDFRAC PDFRAC PDFRAC PDFRAC *** *)
let pdfrac prefs prl = 
  let t = Cmds_common.list_get_same_tree prl
  and pra = Array.of_list prl
  in
  let inda = 
    Array.map
      (Induced.of_placerun 
        (Mokaphy_prefs.criterion_of_bool (Mokaphy_prefs.PDFrac.use_pp prefs)))
      pra
  in
  Cmds_common.wrap_output 
    (Mokaphy_prefs.PDFrac.out_fname prefs) 
    (Cmds_common.write_uptri
      (Mokaphy_prefs.PDFrac.list_output prefs)
      (Array.map Placerun.get_name pra)
      "pdfrac"
      (Uptri.init
        (Array.length inda)
        (fun i j -> Pdfrac.of_induceds t inda.(i) inda.(j))))



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


(* *** CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ CLUSTERVIZ *** *)

(* get mass tree(s) and name them with name *)
let get_named_mass_tree dirname i name = 
  List.flatten
    (List.map
      (fun infix ->
        let fname = 
          dirname^"/"^Cluster_common.mass_trees_dirname
            ^"/"^(Mokaphy_cluster.zeropad i)^infix^".fat.xml" in
        if Sys.file_exists fname then
          [{
            (List.hd
              (Xphyloxml.load fname).Xphyloxml.trees)
            with Xphyloxml.name = Some (name^infix)
          }]
        else [])
      [".phy"; ".tax"])

let clusterviz prefs = function
  | [dirname] -> begin
      match (Mokaphy_prefs.Clusterviz.name_csv prefs,
             Mokaphy_prefs.Clusterviz.out_fname prefs) with
        | "", _ -> failwith "please specify a cluster CSV file"
        | _, "" -> failwith "please specify an out file name"
        | (cluster_fname, out_fname) -> 
          try
            let nameim = Cluster_common.nameim_of_csv cluster_fname 
            and out_tree_name = 
              Cmds_common.chop_suffix_if_present cluster_fname ".csv"
            in
            let (nt, ssm) = Clusterviz.name_tree_and_subsets_map dirname nameim in
            (* write it out, and read it back in for the combination *)
            let ch = open_out out_fname in
            Phyloxml.write_named_tree_list ch [Some out_tree_name, nt];
            close_out ch;
            let named_tree = 
              match (Xphyloxml.load out_fname).Xphyloxml.trees with
              | [t] -> t
              | _ -> assert(false)
            in
            (* now we read in the cluster trees *)
            let mass_trees = 
              List.map 
                (fun (i, name) -> get_named_mass_tree dirname i name)
                (IntMapFuns.to_pairs nameim)
            in
            (* write out the average masses corresponding to the named clusters *)
            Xphyloxml.pxdata_to_file out_fname 
              { Xphyloxml.trees = (named_tree::(List.flatten mass_trees)); 
                Xphyloxml.data_attribs = Xphyloxml.phyloxml_attrs; };
            (* write out CSV file showing the cluster contents *)
            let subsets_fname = Mokaphy_prefs.Clusterviz.subsets_fname prefs in
            if subsets_fname <> "" then 
              Csv.save subsets_fname 
                (StringMap.fold
                  (fun name s l -> 
                    [name; String.concat "," (StringSet.elements s)]::l)
                  ssm
                  [])
          with Clusterviz.Numbering_mismatch ->
            failwith ("numbering mismatch with "^dirname^" and "^cluster_fname)
  end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster directory for clusterviz."


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
