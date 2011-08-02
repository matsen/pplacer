open Batteries
open Guppy_cmdobjs
open Subcommand
open Stree

exception Found_root of stree

let maybe_cons transform = function
  | None -> identity
  | Some x -> transform x |> List.cons
let some x = Some x

let find_root rp =
  let td = Refpkg.get_taxonomy rp
  and gt = Refpkg.get_ref_tree rp
  and seqinfom = Refpkg.get_seqinfom rp in
  let st = Gtree.get_stree gt in
  let node_mrca node =
    node
    |> Stree.leaf_ids
    |> List.map
        (Gtree.get_name gt
         |- Tax_seqinfo.tax_id_by_name seqinfom)
    |> Tax_taxonomy.list_mrca td
  in
  let rec aux ?top_mrca = function
    | Leaf _ as n -> raise (Found_root n)
    | Node (_, subtrees) as n ->
      let subrks = List.map (node_mrca &&& some) subtrees
        |> maybe_cons (None |> (curry identity |> flip)) top_mrca
        |> List.map (Tax_taxonomy.get_tax_rank td |> first)
        |> List.sort
      in
      let at = List.at subrks |- fst in
      if List.length subrks < 2 || at 0 = at 1 then
        raise (Found_root n);
      match List.at subrks 0 with
        | _, Some node ->
          let top_mrca = List.remove subtrees node
            |> List.map node_mrca
            |> maybe_cons identity top_mrca
            |> Tax_taxonomy.list_mrca td
          in
          aux ~top_mrca node
        | _ -> raise (Found_root n)
  in
  aux st


class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  method desc = "reroots a given reference package in place"
  method usage = "usage: reroot -c my.refpkg"

  method action = function
    | [] ->
      let rp = self#get_rp in
      find_root rp

    | _ -> failwith "reroot doesn't take any positional arguments"

end
