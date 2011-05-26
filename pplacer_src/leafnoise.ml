open Fam_batteries
open MapsSets
open Splits

  let distribute_pqueries = Gsl_randist.multinomial rng ~n:n_pqueries in
  StringMap.iter
    (fun name (multiplier, leafss) ->
      List.iter
        (fun i ->
          let leafs = Lsetset.fold Lset.union leafss Lset.empty in
          let distr = Array.to_list
            (distribute_pqueries
               (Array.make (Lset.cardinal leafs) 1.0))
          and leafl = Lset.elements leafs in
          let pqueries =
            List.map2
              (fun leaf -> repeat (pquery_of_leaf_and_seq leaf))
              leafl
              distr
          in
          let pr =
            Placerun.make
              tree
              (Printf.sprintf "commiesim_%s_%d" name i)
              (List.flatten pqueries)
          in
          Placerun_io.to_json_file
            ""
            (Printf.sprintf "%s%s_%d.json" name_prefix name i)
            pr)
        (Base.range multiplier))
    leaf_map;

  (* okay it finally got to me *)
  let next_id = ref (Gtree.top_id cluster_tree)
  and new_bark = ref (Gtree.get_bark_map cluster_tree) in
  let rec aux = function
    | Stree.Node (i, subtrees) -> Stree.node i (List.map aux subtrees)
    | Stree.Leaf i as original ->
      let mult = int_of_float (Gtree.get_bl cluster_tree i) in
      assert(mult >= 1);
      if mult = 1 then original
      else begin
        let subtree =
          Stree.node i
            (List.map
               (fun e ->
                 let old_name = Gtree.get_name cluster_tree i in
                 incr next_id;
                 new_bark :=
                   Newick_bark.map_set_name
                   (!next_id)
                   (Printf.sprintf "%s_%d" old_name e)
                   (!new_bark);
                 Stree.leaf (!next_id))
               (Base.range mult))
        in
        new_bark := IntMap.remove i !new_bark;
        subtree
      end
  in
  let cluster_stree' = aux cluster_stree in
  let new_bark' = !new_bark in
  Gtree.gtree cluster_stree' new_bark'

let random_colored_tree rng size n_colors =
  let st = generate_yule rng size in
  let colors =
    StringSet.of_list
      (List.map
         (fun i -> String.make 1 (char_of_int (i + 65)))
         (Base.range n_colors))
  in
  let choose_color =
    StringSet.plain_sample (sample ~replacement:false rng) colors
  in
  let rec aux accum = function
    | Stree.Leaf i :: rest ->
      let accum' =
        Newick_bark.map_set_name
          i
          (StringSet.choose (choose_color 1)) accum
      in
      aux accum' rest
    | Stree.Node (_, subtrees) :: rest ->
      let rest' = List.rev_append subtrees rest in
      aux accum rest'
    | [] -> accum
  in
  let bark = aux IntMap.empty [st] in
  Gtree.gtree st bark
