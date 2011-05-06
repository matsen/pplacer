open MapsSets
open Splits

let size_transform x = x ** 10.

(* Throw n_items into n_bins such that each bin is not empty. *)
let nonempty_balls_in_boxes rng ~n_bins ~n_items =
  assert(n_items >= n_bins);
  let counts = Array.create n_bins 1 in
  for i=1 to n_items - n_bins do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts

let lsetset_to_array lss =
  let a = Array.make (Lsetset.cardinal lss) (Lsetset.choose lss) in
  let _ = Lsetset.fold (fun x i -> a.(i) <- x; i+1) lss 0 in
  a

(* Throw the elements of lss into n_bins boxes, ensuring that each box has at
 * least one element.
 * The implementation below could be made more functional and list-y but I don't
 * think that it would be shorter.
 * *)
let uniform_nonempty_partition rng n_bins lss =
  let a = lsetset_to_array lss in
  Base.shuffle rng a;
  let pos = ref 0 in
  List.map
    (fun n_samples ->
      let new_lss =
        LsetsetFuns.of_list (Array.to_list (Array.sub a !pos n_samples)) in
      pos := !pos + n_samples;
      new_lss)
    (nonempty_balls_in_boxes rng ~n_bins ~n_items:(Lsetset.cardinal lss))


exception Invalid_sample of string

type weighting =
  | Array of float array
  | Function of (int -> float)
  | Uniform

(* Sampling without replacement.
 * Note that because of the implementation of GSL's discrete distributions, the
 * sum of the elements of an Array weighting need not total to one; it's scaled
 * to make a probability distribution. They do need to be positive. *)
let sample rng ?(weighting = Uniform) n k =
  if k > n then raise (Invalid_sample "k > n");
  if k < 0 then raise (Invalid_sample "k < 0");
  let distribution = match weighting with
    | Array a -> a
    | Function f -> Array.init n f
    | Uniform -> Array.make n 1.0
  in
  let rec aux accum = function
    | 0 -> accum
    | k ->
      let i =
        Gsl_randist.discrete
          rng
          (Gsl_randist.discrete_preproc distribution)
      in
      distribution.(i) <- 0.0;
      aux (i :: accum) (k - 1)
  in
  aux [] k

(* We take the weight on a split to be proportional to weight_transform applied
 * to the size of the smaller element of the set. *)
let sample_sset_weighted rng =
  SsetFuns.weighted_sample
  (fun arr -> sample rng ~weighting:(Array arr))
  (fun (k, _) -> size_transform (float_of_int (Lset.cardinal k)))

let repeat f n =
  let rec aux accum = function
    | 0 -> accum
    | n -> aux ((f n) :: accum) (n - 1)
  in
  aux [] n

let rev_enumerate ?(start = 0) l =
  let _, l' = List.fold_left
    (fun (e, accum) x -> e + 1, (e, x) :: accum)
    (start, [])
    l
  in
  l'

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
  aux (count + 1) (repeat Stree.leaf count)


let newick_bark_of_prefixed_int prefix n =
  Newick_bark.map_set_name n (Printf.sprintf "%s%d" prefix n) IntMap.empty

let rec bark_of_stree_numbers bark_fn = function
  | Stree.Leaf n -> bark_fn n
  | Stree.Node (_, subtree) ->
    List.fold_left
      (fun map node ->
        IntMapFuns.union map (bark_of_stree_numbers bark_fn node))
      IntMap.empty
      subtree

let gtree_of_stree_numbers bark_fn stree =
  let bark = bark_of_stree_numbers bark_fn stree in
  Gtree.gtree stree bark

(* generate the root distribution *)
let generate_root rng include_prob poisson_mean ?(min_leafs = 0) splits leafs =
  let k =
    max
      (min_leafs - 1)
      (Gsl_randist.poisson rng poisson_mean)
  in
  let splits' = sample_sset_weighted rng splits k in
  let leafss = sset_lsetset splits' (Lsetset.singleton leafs) in
  let base_leafs = LsetsetFuns.uniform_sample (sample rng) leafss min_leafs in
  let to_sample =
    Lsetset.filter
      (fun lset -> not (Lsetset.mem lset base_leafs))
      leafss
  in
  Lsetset.union
    base_leafs
    (Lsetset.filter
       (fun _ -> Gsl_rng.uniform rng < include_prob)
       to_sample)

let rec distribute_lsetset_on_stree rng poisson_mean splits leafss = function
  | Stree.Leaf n -> IntMap.add n leafss IntMap.empty
  | Stree.Node (_, subtree) ->
    let n_splits = Gsl_randist.poisson rng poisson_mean in
    let splits' = sample_sset_weighted rng splits n_splits in
    let leafss' = sset_lsetset splits' leafss in
    let distributed =
      uniform_nonempty_partition rng (List.length subtree) leafss'
    in
    List.fold_left2
      (fun map leafss node ->
        IntMapFuns.union
          map
          (distribute_lsetset_on_stree rng poisson_mean splits leafss node))
      IntMap.empty
      distributed
      subtree

let pquery_of_leaf_and_seq leaf seq =
  Pquery.make_ml_sorted
    [Printf.sprintf "%d_%d" leaf seq]
    ""
    [
      Placement.make_ml
        leaf
        ~ml_ratio:1.0
        ~dist_bl:0.0
        ~pend_bl:1.0
        ~log_like:0.0
    ]

let main
    rng
    ?include_prob
    ~poisson_mean
    ?(retries = 100)
    ~yule_size
    ~n_pqueries
    ~tree
    ~name_prefix =

  let stree = Gtree.get_stree tree in
  let splits = sset_of_tree stree
  and leafs = lset_of_tree stree in

  let rec retry = function
    | 0 -> failwith "failed too many resamplings"
    | n ->
      let cluster_tree = generate_yule rng yule_size in
      try
        let leafss =
          match include_prob with
          | None -> Lsetset.singleton leafs
          | Some include_prob ->
            generate_root
              rng
              include_prob
              poisson_mean
              ~min_leafs:yule_size
              splits
              leafs
        in
        let map = distribute_lsetset_on_stree rng poisson_mean splits leafss cluster_tree in
        cluster_tree, map
      with
        | Invalid_sample _ -> retry (n - 1)
  in
  let cluster_tree, leaf_map = retry retries in

  let distribute_pqueries = Gsl_randist.multinomial rng ~n:n_pqueries in
  IntMap.iter
    (fun e leafss ->
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
          (Printf.sprintf "commiesim_%d" e)
          (List.flatten pqueries)
      in
      Placerun_io.to_json_file
        ""
        (Printf.sprintf "%s%d.json" name_prefix e)
        pr)
    leaf_map;

  let cluster_gtree = gtree_of_stree_numbers
    (newick_bark_of_prefixed_int name_prefix)
    cluster_tree
  in
  Newick_gtree.to_file cluster_gtree (name_prefix ^ ".tre")
