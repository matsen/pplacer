open Ppatteries
module SSS = Squash_common.StringSetSet

(* Read in two files: boot_fname, which contains a collection of bootstrapped
 * trees, and ct_fname, which is the tree with the full data.
 * Write out an XML file with those bootstrap values. *)
let decorate_tree cutoff boot_fname ct_fname =
  let ct = Squash_common.ensure_numbered (Newick_gtree.of_file ct_fname)
  and boot_tl = Newick_gtree.list_of_file boot_fname
  in
  let boot_sssl = List.map Squash_common.sss_of_tree boot_tl
  and taxon_list t = List.map (Gtree.get_name t) (Gtree.leaf_ids t)
  in
  let taxon_set t = List.fold_right StringSet.add (taxon_list t) StringSet.empty
  in
  let taxs = taxon_set ct
  and boot_ssl = List.map taxon_set boot_tl
  in
  List.iter
    (fun ss -> if 0 <> StringSet.compare taxs ss then
                 invalid_arg "taxon sets not identical")
    boot_ssl;
  (* we count up bootstraps for a clade by seeing if that same taxon subset
   * exists in the bootstrap set set list. *)
  let bootval_im =
    IntMap.map
      (fun ss ->
        List.fold_right
          (fun bsss -> ( + ) (if SSS.mem ss bsss then 1 else 0))
          boot_sssl
          0)
    (Squash_common.ssim_of_tree ct)
  in
  Gtree.set_bark_map
    ct
    (IntMap.map
      (fun b ->
        match b#get_name_opt with
        | Some _ -> b
        | None ->
            (let cnode_num = int_of_float b#get_boot in
              (b#set_name (string_of_int cnode_num))
                #set_boot_opt
                  (let boot_val =
                    float_of_int (IntMap.find cnode_num bootval_im) in
                  if boot_val >= cutoff then Some boot_val
                  else None)))
      (Gtree.get_bark_map ct))
