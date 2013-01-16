open Ppatteries
open Guppy_cmdobjs
open Subcommand

open Tax_id
open Stree

exception Found_root of stree

(* taxonomic rerooting of a tree, assuming all leaf labels are sequence names
 * in the reference packages. *)
let find_root rp gt =
  let td = Refpkg.get_taxonomy rp
  and seqinfom = Refpkg.get_seqinfom rp in
  let st = Gtree.get_stree gt in
  let node_mrca node =
    node
    |> Stree.leaf_ids
    |> List.filter_map
        (fun leaf ->
          try
            Gtree.get_node_label gt leaf
            |> Tax_seqinfo.tax_id_by_node_label seqinfom
            |> junction ((=) NoTax) (const None) some
          with Gtree.Lacking_bark _ -> None)
    |> junction
        List.is_empty
        (const None)
        (Tax_taxonomy.list_mrca td |- some)
  in
  let rec aux: ?top_mrca:Tax_id.t -> stree -> unit = fun ?top_mrca -> function
    | Leaf _ -> failwith "tried to reroot at a leaf"
    | Node (_, subtrees) as n ->
      (* walk the tree, and at each node, find the MRCA of all of the leaves
       * below that node, then the rank of each MRCA. if we're not at the root
       * node, we'll also be considering the MRCA of everything above us on
       * the tree via top_mrca. *)
      let subrks = subtrees
        |> List.filter_map
            (fun node -> match node_mrca node with
              | Some mrca -> Some (mrca, Some node)
              | None -> None)
        |> maybe_map_cons (None |> (curry identity |> flip)) top_mrca
        |> List.map (Tax_taxonomy.get_tax_rank td |> Tuple.Tuple2.map1)
        |> List.sort compare
      in
      let at = List.at subrks |- fst in
      (* if the two highest ranks are equal, we've found the root. *)
      if List.length subrks > 1 && at 0 = at 1 then
        raise (Found_root n);
      (* otherwise, descend toward the node with the highest MRCA rank. this
       * will never ascend the tree, as the entry representing "the tree above
       * us" is None. if the highest MRCA rank is a leaf, stop here. *)
      match subrks with
        | (_, Some (Node _ as node)) :: _ ->
          (* find the MRCA of everything above the selected node in the entire
           * tree by taking the MRCA of the previous MRCA and the other
           * subtrees (which will then be above us at the next node). *)
          let top_mrca = List.remove subtrees node
            |> List.filter_map node_mrca
            |> maybe_cons top_mrca
            |> junction
                List.is_empty
                (const None)
                (Tax_taxonomy.list_mrca td |- some)
          in
          aux ?top_mrca node
        | _ -> raise (Found_root n)
  in
  try
    aux st; failwith "no root found?"
  with Found_root root -> top_id root


class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit output_cmd () as super_output

  method specl = super_refpkg#specl @ super_output#specl

  method desc = "reroots a given reference package in place"
  method usage = "usage: reroot -c my.refpkg"

  method private rerooted_tree lbl =
    let rp = self#get_rp in
    let gt = Refpkg.get_ref_tree rp in
    let st = gt.Gtree.stree
    and td = Refpkg.get_taxonomy rp in
    (* first, determine the highest rank that has more than one tax_id. *)
    let rank_tax_map = Convex.rank_tax_map_of_refpkg rp in
    let rank, taxmap =
      try
        Enum.find
          (snd |- IntMap.values |- TaxIdSet.of_enum |- TaxIdSet.cardinal |- (<) 1)
          (IntMap.enum rank_tax_map)
      with Not_found ->
        dprint "ref tree has <= 1 taxon; writing unmodified tree\n";
        Return.return lbl gt
    in
    Tax_taxonomy.get_rank_name td rank
      |> dprintf "rerooting at %s\n";
    (* next, find the convex subset of leaves at that rank. *)
    let phi, _ = Convex.solve (taxmap, st) in
    let not_cut = Convex.nodeset_of_phi_and_tree phi st in
    (* reroot after pruning the tree down to that convex subset of leaves. *)
    Gtree.get_bark_map gt
      |> IntMap.filter (flip IntSet.mem not_cut |> const |> flip)
      |> Gtree.set_bark_map gt
      |> find_root rp
      |> tap (dprintf "root found at node %d\n")
      |> Gtree.reroot gt

  method action = function
    | [] ->
      Newick_gtree.to_file
        (Return.label self#rerooted_tree)
        (self#single_file ())

    | _ -> failwith "reroot doesn't take any positional arguments"

end
