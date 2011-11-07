open Ppatteries
open Guppy_cmdobjs
open Subcommand

open Tax_id
open Stree

exception Found_root of stree

let some x = Some x

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
            |> some
          with Gtree.Lacking_bark _ -> None)
    |> junction
        List.is_empty
        (const None)
        (Tax_taxonomy.list_mrca td |- some)
  in
  let rec aux: ?top_mrca:Tax_id.t -> stree -> unit = fun ?top_mrca -> function
    | Leaf _ as n -> raise (Found_root n)
    | Node (_, subtrees) as n ->
      let subrks = subtrees
        |> List.filter_map
            (fun node -> match node_mrca node with
              | Some mrca -> Some (mrca, Some node)
              | None -> None)
        |> maybe_map_cons (None |> (curry identity |> flip)) top_mrca
        |> List.map (Tax_taxonomy.get_tax_rank td |> first)
        |> List.sort compare
      in
      let at = List.at subrks |- fst in
      if List.length subrks < 2 || at 0 = at 1 then
        raise (Found_root n);
      match List.at subrks 0 with
        | _, Some node ->
          let top_mrca = List.remove subtrees node
            |> List.filter_map node_mrca
            |> maybe_cons top_mrca
            |> Tax_taxonomy.list_mrca td
          in
          aux ~top_mrca node
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

  method action = function
    | [] ->
      let rp = self#get_rp in
      let gt = Refpkg.get_ref_tree rp in
      let st = gt.Gtree.stree
      and td = Refpkg.get_taxonomy rp in
      let rank_tax_map = Convex.rank_tax_map_of_refpkg rp in
      let rank, taxmap = Enum.find
        (snd |- IntMap.values |- TaxIdSet.of_enum |- TaxIdSet.cardinal |- (<) 1)
        (IntMap.enum rank_tax_map)
      in
      Tax_taxonomy.get_rank_name td rank
        |> Printf.sprintf "rerooting at %s"
        |> print_endline;
      let phi, _ = Convex.solve (taxmap, st) in
      let not_cut = Convex.nodeset_of_phi_and_tree phi st in
      Gtree.get_bark_map gt
        |> IntMap.filteri (flip IntSet.mem not_cut |> const |> flip)
        |> Gtree.set_bark_map gt
        |> find_root rp
        |> Gtree.reroot gt
        |> flip Newick_gtree.to_file (self#single_file ())

    | _ -> failwith "reroot doesn't take any positional arguments"

end
