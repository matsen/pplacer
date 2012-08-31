open Ppatteries

type 'a t = {
  map: Tax_id.t IntMap.t;
  tree: 'a Gtree.gtree;
}
type kind = Mrca_map | Edge_painting

let kind_of_mrca_class = function
  | true -> Mrca_map
  | false -> Edge_painting

let map {map} = map
let tree {tree} = tree

(* Fill in the normally-sparse MRCA map so that every node in the tree maps to
 * the appopriate MRCA, instead of just the nodes where the MRCAs occur. *)
let all_mrcas rp =
  let mrcam = Refpkg.get_mrcam rp
  and utm = Refpkg.get_uptree_map rp in
  let rec update mrcam i =
    match IntMap.Exceptionless.find i mrcam with
      | Some x -> mrcam, x
      | None ->
        let mrcam', x = update mrcam (IntMap.find i utm) in
        IntMap.add i x mrcam', x
  in
  Refpkg.get_ref_tree rp
    |> Gtree.get_stree
    |> Stree.node_ids
    |> List.fold_left (update |-- fst) mrcam

let of_refpkg rp = function
  | Mrca_map -> {map = all_mrcas rp; tree = Refpkg.get_ref_tree rp}
  | Edge_painting ->
    {map = Edge_painting.of_refpkg rp; tree = Refpkg.get_ref_tree rp}

let of_refpkg_mrca_class rp mrca_class =
  of_refpkg rp (kind_of_mrca_class mrca_class)

let root_taxon cm =
  IntMap.find (Gtree.top_id cm.tree) cm.map

let classify cm p =
  IntMap.get (Placement.location p) Tax_id.NoTax cm.map

let classify_strict cm p =
  IntMap.find (Placement.location p) cm.map
