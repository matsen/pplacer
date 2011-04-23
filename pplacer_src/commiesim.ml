module G = Gsl_rng

let sample rng n =
  let rec aux accum = function
    | 0 -> accum
    | k ->
      let rec select () =
        match Gsl_rng.uniform_int rng n with
          | i when List.mem i accum -> select ()
          | i -> i
      in
      aux ((select ()) :: accum) (k - 1)
  in
  aux []

let repeat f n =
  let rec aux accum = function
    | -1 -> accum
    | n -> aux ((f n) :: accum) (n - 1)
  in
  aux [] (n - 1)

let generate_yule rng count =
  let rec aux next_id trees =
    match List.length trees with
      | 1 -> List.hd trees
      | n ->
        let nodes = sample rng n 2 in
        let _, trees', nodes' = List.fold_left
          (fun (e, l1, l2) x ->
            if List.mem e nodes then
              e + 1, l1, x :: l2
            else
              e + 1, x :: l1, l2)
          (0, [], [])
          trees
        in
        aux
          (next_id + 1)
          (Stree.node next_id nodes' :: trees')
  in
  aux count (repeat Stree.leaf count)
