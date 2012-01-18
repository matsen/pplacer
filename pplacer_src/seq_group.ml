open Ppatteries

type bin = {
  mutable lhs: int;
  mutable rhs: int;
}
let bin division i = {lhs = i * division; rhs = pred (succ i * division)}
let update b l r = b.lhs <- min b.lhs l; b.rhs <- max b.rhs r
let lhs {lhs} = lhs
let rhs {rhs} = rhs

let group n_groups sl =
  let sites = List.hd sl |> snd |> String.length in
  let division = sites / n_groups in
  let bins = Array.init n_groups (bin division)
  and grouped = Array.make n_groups []
  and spanned_seqs = List.map ((snd |- Alignment.span) &&& identity) sl in
  let group_mv_dist l r group =
    let {lhs; rhs} = bins.(group) in
    (lhs - min l lhs) + (max r rhs - rhs)
  in
  let best_mv_dist ((l, r), _) =
    0 --^ n_groups
      |> Enum.map (identity &&& group_mv_dist l r)
      |> Enum.arg_min snd
  in
  let rec aux sl =
    let used = List.map (best_mv_dist &&& identity) sl
      |> List.group (comparing (fst |- fst))
      |> List.map (List.enum |- Enum.arg_min (fst |- snd))
      |> List.fold_left
          (fun accum ((group, _), ((l, r), (name, _ as row))) ->
            update bins.(group) l r;
            grouped.(group) <- row :: grouped.(group);
            StringSet.add name accum)
          StringSet.empty
    in
    match List.filter (snd |- fst |- flip StringSet.mem used |- not) sl with
      | [] -> ()
      | sl' -> aux sl'
  in
  aux spanned_seqs;
  grouped
