open Ppatteries
open Convex
open Stree

let of_refpkg rp =
  let gt = Refpkg.get_ref_tree rp in
  let st = Gtree.get_stree gt in
  let rankmap = rank_tax_map_of_refpkg rp
    |> IntMap.map (flip (curry build_sizemim_and_cutsetim) st |- snd)
  in
  let highest_rank, _ = IntMap.max_binding rankmap in
  node_ids st
    |> List.enum
    |> Enum.filter ((<>) (top_id st))
    |> Enum.map
        (fun i ->
          let rec aux rank =
            (* XXX this needs checking by Aaron.
             * We skip things that don't appear in the rank map. *)
            if IntMap.mem rank rankmap then begin
              let cset = IntMap.find rank rankmap |> IntMap.find i in
              if ColorSet.cardinal cset = 1 then
                i, ColorSet.choose cset
              else if rank = 0 then
                failwith (Printf.sprintf "couldn't evaluate %d" i)
              else
                aux (rank - 1)
            end
            else aux (rank - 1)
          in
          aux highest_rank)
    |> IntMap.of_enum
