
module Prefs = struct
  type mokaphy_prefs =
    {
      out_fname: string ref;
      name_csv: string ref;
      subsets_fname: string ref;
    }

  let out_fname p = !(p.out_fname)
  let name_csv p = !(p.name_csv)
  let subsets_fname p = !(p.subsets_fname)

  let defaults () =
    {
      out_fname = ref "";
      name_csv = ref "";
      subsets_fname = ref "";
    }

  (* arguments *)
  let specl_of_prefs prefs = [
    "-o", Arg.Set_string prefs.out_fname,
    "Specify a prefix for the clusters (required).";
    "--name-csv", Arg.Set_string prefs.name_csv,
    "A CSV file containing two columns: \"numbers\", which are node numbers in the clustered tree, and \"names\", which are names for those nodes.";
    "--subsets", Arg.Set_string prefs.subsets_fname,
    "Specify a file to write out subset membership.";
    ]
end


open MapsSets

exception Numbering_mismatch


(* makes a map from node labels (in bootstrap positions) to the node numbers *)
let nodemap_of_tree t =
  IntMap.fold
    (fun i b ->
      match b#get_boot_opt with
      | None -> fun m -> m
      | Some boot -> begin
          if boot <> float_of_int (int_of_float boot) then
            invalid_arg "non-integer label for a purported cluster tree";
          IntMap.add (int_of_float boot) i
        end)
    (Gtree.get_bark_map t)
    IntMap.empty

(* given a tree with node numbering in the bootstrap location, and a map from
 * those numbers to strings, naming those nodes, make a tree with names in the
 * appropriate locations and no bootstraps. *)
let make_named_tree sm t =
  Gtree.set_bark_map t
    (IntMap.map
      (fun b ->
        match b#get_boot_opt with
        | None -> b
        | Some boot ->
          let no_boot_b = b#set_boot_opt None
          and node_id = int_of_float boot in
          if not (IntMap.mem node_id sm) then no_boot_b
          else no_boot_b#set_name (IntMap.find node_id sm))
      (Gtree.get_bark_map t))

let name_tree_and_subsets_map dirname nameim =
  let t = Newick_gtree.of_file (Cluster_common.tree_name_of_dirname dirname) in
  let nodeim = nodemap_of_tree t in
  (* shifted_nameim uses the Stree numbering rather than that given by
   * the bootstrap labels (as nameim does) *)
  try
    let shifted_nameim =
      IntMap.fold
        (fun cluster_num name ->
          IntMap.add (IntMap.find cluster_num nodeim) name)
        IntMap.empty
        nameim
    in
    let ssim = Cluster_common.ssim_of_tree t in
    (make_named_tree shifted_nameim t,
      IntMap.fold
        (fun cluster_num name -> StringMap.add name (IntMap.find cluster_num ssim))
        nameim
        StringMap.empty)
  with
  | Not_found -> raise Numbering_mismatch


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
              (Phyloxml.load fname).Phyloxml.trees)
            with Phyloxml.name = Some (name^infix)
          }]
        else [])
      [".phy"; ".tax"])

let clusterviz prefs = function
  | [dirname] -> begin
      match (Prefs.name_csv prefs,
             Prefs.out_fname prefs) with
        | "", _ -> failwith "please specify a cluster CSV file"
        | _, "" -> failwith "please specify an out file name"
        | (cluster_fname, out_fname) ->
          try
            let nameim = Cluster_common.nameim_of_csv cluster_fname in
            let (nt, ssm) = name_tree_and_subsets_map dirname nameim in
            (* write it out, and read it back in for the combination
            Phyloxml.named_gtree_to_file out_fname out_tree_name nt;
            let named_tree =
              match (Phyloxml.load out_fname).Phyloxml.trees with
              | [t] -> t
              | _ -> assert(false)
            in
*)
            let cluster_pxtree =
              Phyloxml.pxtree_of_gtree
                ~name:
                  (Mokaphy_common.chop_suffix_if_present cluster_fname ".csv")
                nt
            (* now we read in the cluster trees *)
            and mass_trees =
              List.map
                (fun (i, name) -> get_named_mass_tree dirname i name)
                (IntMapFuns.to_pairs nameim)
            in
            (* write out the average masses corresponding to the named clusters *)
            Phyloxml.pxdata_to_file out_fname
              { Phyloxml.trees = (cluster_pxtree::(List.flatten mass_trees));
                Phyloxml.data_attribs = Phyloxml.phyloxml_attrs; };
            (* write out CSV file showing the cluster contents *)
            let subsets_fname = Prefs.subsets_fname prefs in
            if subsets_fname <> "" then
              Csv.save subsets_fname
                (StringMap.fold
                  (fun name s l ->
                    [name; String.concat "," (StringSet.elements s)]::l)
                  ssm
                  [])
          with Numbering_mismatch ->
            failwith ("numbering mismatch with "^dirname^" and "^cluster_fname)
  end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster directory for clusterviz."

