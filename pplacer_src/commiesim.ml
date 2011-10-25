open Ppatteries
open Splits

let size_transform x = x ** 10.

(* Return the counts from throwing n_items into n_bins. *)
let balls_in_boxes rng ~n_bins ~n_items =
  let counts = Array.create n_bins 0 in
  for i=1 to n_items do
    let which_bin = Gsl_rng.uniform_int rng n_bins in
    counts.(which_bin) <- counts.(which_bin) + 1
  done;
  Array.to_list counts

(* Return the counts from throwing n_items into n_bins such that each bin is not
 * empty. *)
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
  shuffle rng a;
  let pos = ref 0 in
  List.map
    (fun n_samples ->
      let new_lss =
        Lsetset.of_list (Array.to_list (Array.sub a !pos n_samples)) in
      pos := !pos + n_samples;
      new_lss)
    (nonempty_balls_in_boxes rng ~n_bins ~n_items:(Lsetset.cardinal lss))

exception Invalid_sample of string

type weighting =
  | Array of float array
  | Function of (int -> float)
  | Uniform

(* Sampling with and without replacement.
 * Note that because of the implementation of GSL's discrete distributions, the
 * sum of the elements of an Array weighting need not total to one; it's scaled
 * to make a probability distribution. They do need to be positive. *)
let sample ~replacement rng ?(weighting = Uniform) n k =
  if k > n then raise (Invalid_sample "k > n");
  if k < 0 then raise (Invalid_sample "k < 0");
  let distribution = match weighting with
    | Array a -> a
    | Function f -> Array.init n f
    | Uniform -> Array.make n 1.0
  in
  if k = 0 then []
  else begin
    let discrete_distn = Gsl_randist.discrete_preproc distribution in
    let rec aux_repl accum = function
      | 0 -> accum
      | k ->
        aux_repl ((Gsl_randist.discrete rng discrete_distn) :: accum) (k - 1)
    and aux_no_repl accum = function
      | 0 -> accum
      | k ->
        let i =
          Gsl_randist.discrete
            rng
            (Gsl_randist.discrete_preproc distribution)
        in
        (* clear out i from the distribution (w/o replacement *)
        distribution.(i) <- 0.0;
        aux_no_repl (i :: accum) (k - 1)
    in
    (if replacement then aux_repl else aux_no_repl) [] k
  end

(* We take the weight on a split to be proportional to weight_transform applied
 * to the size of the smaller element of the set. *)
let sample_sset_weighted rng =
  Sset.weighted_sample
    (fun arr -> sample ~replacement:true rng ~weighting:(Array arr))
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
        let nodes = sample ~replacement:false rng n 2 in
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
    |> Stree.boost (-1)

let newick_bark_of_prefixed_int prefix n =
  Newick_bark.map_set_node_label n (Printf.sprintf "%s%d" prefix n) IntMap.empty

let rec bark_of_stree_numbers bark_fn = function
  | Stree.Leaf n -> bark_fn n
  | Stree.Node (_, subtree) ->
    List.fold_left
      (fun map node ->
        IntMap.union map (bark_of_stree_numbers bark_fn node))
      IntMap.empty
      subtree

let gtree_of_stree_numbers bark_fn stree =
  let bark = bark_of_stree_numbers bark_fn stree in
  Gtree.gtree stree bark

let generate_lengthy_tree rng ~a ~b count =
  let empty = new Newick_bark.newick_bark `Empty
  and st = generate_yule rng count in
  let leaves = Stree.leaf_ids st |> IntSet.of_list in
  let bark = Enum.from (fun () -> Gsl_randist.gamma rng ~a ~b)
    |> Enum.map empty#set_bl
    |> Enum.take (Stree.top_id st)
    |> Enum.mapi
        (fun i b -> i,
          if IntSet.mem i leaves then
            Printf.sprintf "n%d" i |> b#set_node_label
          else b)
    |> IntMap.of_enum
    |> IntMap.add (Stree.top_id st) (empty#set_bl 0.)
  in
  Gtree.gtree st bark

let generate_caterpillar_tree rng ~mu count =
  let rec aux accum = function
    | 1 -> accum
    | n ->
      let top = Stree.top_id accum in
      aux
        (Stree.node (top + 2) [Stree.Leaf (top + 1); accum])
        (pred n)
  in
  let st = aux (Stree.Leaf 0) count in
  let bark = 0 --^ (Stree.top_id st)
    |> Enum.map (identity &&& (fun _ -> Gsl_randist.exponential rng ~mu |> Newick_bark.empty#set_bl))
    |> IntMap.of_enum
  in
  Gtree.gtree st bark

(* Independent samples. *)
let subselect rng n_select n_bins lss =
  List.init
    n_bins
    (fun _ ->
      Lsetset.plain_sample
        (sample ~replacement:true rng ~weighting:Uniform)
        lss
        n_select)

let fill n x = List.init n (fun _ -> x)

(* With probability p_same, select identical sets for each, and if not, then
  * independent. *)
let some_matching rng n_select p_same n_bins lss =
  if p_same > 1. || p_same < 0. then
    invalid_arg "p_same out of range";
  let sample n =
    Lsetset.plain_sample
      (sample ~replacement:true rng ~weighting:Uniform)
      lss
      n
  in
  let n_same = Gsl_randist.binomial rng p_same n_select in
  List.map2
    Lsetset.union
      (fill n_bins (sample n_same))
      (List.init n_bins (fun _ -> sample (n_select - n_same)))

let distribute_lsetset_on_tree rng n_select n_splits_mean splits leafss gt =
  let bl = Gtree.get_bl gt in
  let name = Gtree.get_node_label gt in
  let rec aux splits leafss = function
    | Stree.Leaf n ->
      StringMap.add (name n) leafss StringMap.empty
    | Stree.Node (n, subtrees) ->
      (* the splits that actually cut leafss *)
      let cutting_splits = select_sset_cutting_lsetset splits leafss in
      (* sample some number from them *)
      let n_splits = Gsl_randist.poisson rng n_splits_mean in
      Printf.printf "using %d splits\n" n_splits;
      let chosen_splits = sample_sset_weighted rng cutting_splits n_splits in
      (* apply these splits *)
      let cut_leafss = sset_lsetset chosen_splits leafss in
      (* throw the balls (leafs) into boxes (subtrees) *)
      let distributed =
        some_matching rng n_select (bl n) (List.length subtrees) cut_leafss
      in
      List.fold_left2
        (fun map leafss t -> StringMap.union map (aux cutting_splits leafss t))
        StringMap.empty
        distributed
        subtrees
  in
  aux splits leafss gt.Gtree.stree

let pquery_of_leaf_and_seq leaf ?(dist_bl = 0.0) seq =
  Pquery.make_ml_sorted
    [Printf.sprintf "%d_%d" leaf seq]
    ""
    [
      Placement.make_ml
        leaf
        ~ml_ratio:1.0
        ~dist_bl
        ~pend_bl:1.0
        ~log_like:0.0
    ]

(* Write a placerun with pqueries distributed across the branch length on the tree. *)
let write_random_lengthy_pr rng tree name n_pqueries =
  0 --^ Gtree.top_id tree
  |> Enum.map (Gtree.get_bl tree)
  |> Array.of_enum
  |> (fun p -> Gsl_randist.multinomial rng ~n:n_pqueries ~p)
  |> Array.enum
  |> Enum.mapi
      (fun n count ->
        let bl = Gtree.get_bl tree n in
        Enum.init
          count
          (fun x -> pquery_of_leaf_and_seq n ~dist_bl:(Gsl_rng.uniform rng *. bl) x))
  |> Enum.flatten
  |> List.of_enum
  |> Placerun.make tree name
  |> Placerun_io.to_json_file "" (name ^ ".jplace")

(* Write a placerun with pqueries uniformly distributed among the leaves in leafl. *)
let write_random_pr rng tree leafl name n_pqueries =
  let distribute_pqueries = Gsl_randist.multinomial rng ~n:n_pqueries in
  let pqueries =
    List.map2
      (fun leaf -> repeat (pquery_of_leaf_and_seq leaf))
      leafl
      (Array.to_list
        (distribute_pqueries (Array.make (List.length leafl) 1.0)))
  in
  Placerun_io.to_json_file
    ""
    (name^".jplace")
    (Placerun.make tree name (List.flatten pqueries))

(* Uniformly select n_locations from leafl and pass off to write_random_pr. *)
let write_clustered_random_pr rng tree leafl name ~n_locations ~n_pqueries =
  let leaf_subl =
    IntSet.elements
      (IntSet.plain_sample
        (sample ~replacement:false rng ~weighting:Uniform)
        (IntSet.of_list leafl)
        n_locations)
  in
  write_random_pr rng tree leaf_subl name n_pqueries


let main
    rng
    ?(retries = 100)
    ~n_select
    ~n_splits_mean
    ~cluster_tree
    ~n_pqueries
    ~tree
    name_prefix =

  let stree = Gtree.get_stree tree in
  let splits = sset_of_tree stree
  and leafs = lset_of_tree stree
  in

  let rec retry = function
    | 0 -> failwith "failed too many resamplings"
    | n ->
      try
        distribute_lsetset_on_tree
          rng
          n_select
          n_splits_mean
          splits
          (Lsetset.singleton leafs)
          cluster_tree
      with
        | Invalid_sample _ -> retry (n - 1)
  in
  let leaf_map = retry retries in

  StringMap.iter
    (fun name leafss ->
      let leafs = Lsetset.fold Lset.union leafss Lset.empty in
      write_random_pr
        rng
        tree
        (IntSet.elements leafs)
        (Printf.sprintf "%s%s" name_prefix name)
        n_pqueries)
    leaf_map


let random_colored_tree rng size n_colors =
  let st = generate_yule rng size in
  let colors = 0 --^ n_colors
    |> Enum.map (fun i -> String.make 1 (char_of_int (i + 65)))
    |> StringSet.of_enum
  in
  let choose_color =
    StringSet.plain_sample (sample ~replacement:false rng) colors
  in
  let rec aux accum = function
    | Stree.Leaf i :: rest ->
      let accum' =
        Newick_bark.map_set_node_label
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
