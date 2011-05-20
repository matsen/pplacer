open Tax_id
open MapsSets

exception Multiple_roots of tax_id * tax_id
exception No_root


(* get the most distal taxa in the taxonomy which are represented in til.
 * preserve the order of til as much as possible *)
let tax_tips_of_tax_list td til =
  (* s is the set of things we have seen, accu is our list of tips *)
  let rec aux s accu = function
    | x::l ->
        if TaxIdSet.mem x s then aux s accu l
        else begin
          let lin = Tax_taxonomy.get_lineage td x in
          let removes = TaxIdSet.of_list lin in
          (* below: we add the lineage of our taxonomy
           * and take our ancestors from accu if present *)
          aux
            (List.fold_right TaxIdSet.add lin s)
            (x::(List.filter (fun x -> not (TaxIdSet.mem x removes)) accu))
            l
        end
    | [] -> accu
  in
  List.rev (aux TaxIdSet.empty [] til)

(* now we build a tree represented by a series of maps from ancestors to
 * descendants *)
let build_topdown_tree td tips =
  (* rooto is the root of the tree, if its been found *)
  let rec add_ancestry rooto tt ti =
    if Tax_taxonomy.has_ancestor td ti then begin
      let anc = Tax_taxonomy.get_ancestor td ti in
      let tt' = TaxIdMap.add_listly anc ti tt in
      (* if anc was already in tt then we don't have to add its lineage *)
      if TaxIdMap.mem anc tt then (rooto, tt')
      else add_ancestry rooto tt' anc
    end
    else match rooto with
    | Some root ->
        if root = ti then (rooto, tt) else raise (Multiple_roots (root, ti))
    | None -> (Some ti, tt)
  in
  let rec aux rooto tt = function
    | ti::l ->
        assert(not(TaxIdMap.mem ti tt)); (* should be nonredundant list *)
        let (rooto', tt') = add_ancestry rooto tt ti in
        aux rooto' (TaxIdMap.add ti [] tt') l (* add ti itself *)
    | [] -> (rooto, tt)
  in
  match aux None TaxIdMap.empty tips with
  | (Some root, tt) -> (root, TaxIdMap.map List.rev tt)
  | (None, _) -> raise No_root


(* the map is a map from edge numbers to taxids *)
let stree_and_map_of_topdown_tree root tt =
  let m = ref IntMap.empty in
  let count = ref (-1) in
  (* side effects heh heh *)
  let add ti =
    incr count;
    m := IntMap.add (!count) ti !m
  in
  (* note that the order of events below is important *)
  let rec aux ti =
    match TaxIdMap.find ti tt with
    | [] -> add ti; Stree.Leaf (!count)
    | below ->
        let tL = List.map aux below in
        add ti;
        Stree.Node(!count, tL)
  in
  let t = aux root in
  (t, !m)

(* also get a map from the indices of the tree to tax_ids *)
let decor_gtree_of_topdown_tree bl_of_rank td root tt =
  let bl_of_taxid ti = bl_of_rank (Tax_taxonomy.get_tax_rank td ti) in
  let (stree, ti_imap) = stree_and_map_of_topdown_tree root tt in
  let bark_of_taxid ti =
        new Decor_bark.decor_bark
          (`Of_bl_name_boot_dlist
            (Some (bl_of_taxid ti), None, None,
            [Decor.Taxinfo (ti, Tax_taxonomy.get_tax_name td ti)]))
  in
  (Gtree.gtree
    stree
    (IntMap.map bark_of_taxid ti_imap),
  ti_imap)

(* general make a tax_gtree out of Tax_taxonomy.t and a tax_id list *)
let build_gen bl_of_rank td til =
  let (root, tt) = build_topdown_tree td (tax_tips_of_tax_list td til) in
  decor_gtree_of_topdown_tree bl_of_rank td root tt

let constant_bl c _ = c
let unit_bl = constant_bl 1.
let inverse i = 1. /. (float_of_int (i+1))

let build_unit = build_gen unit_bl
let build_inverse = build_gen inverse


(* *** Refpkg interface *** *)
(* can be pulled out if desired later *)

let of_refpkg_gen bl_of_rank rp =
  let td = Refpkg.get_taxonomy rp
  and sim = Refpkg.get_seqinfom rp
  and t = Refpkg.get_ref_tree rp
  in
  build_gen
    bl_of_rank
    td
    (List.map
      (fun id -> Tax_seqinfo.tax_id_by_name sim (Gtree.get_name t id))
      (Gtree.leaf_ids t))

let of_refpkg_unit = of_refpkg_gen unit_bl
let of_refpkg_inverse = of_refpkg_gen inverse

