(*
 * here we find clusters for Sujatha.
 * we take the 
 *)

open MapsSets

let t1 = Newick.of_file "nontrivial_cluster.tre"
let t2 = Newick.of_file "tax_nontrivial_cluster.tre"

let check_add x s =
  if StringSet.mem x s then invalid_arg "check_add"
  else StringSet.add x s

let disj_union s1 s2 =
  if StringSet.empty = StringSet.inter s1 s2 then 
    StringSet.union s1 s2
  else
    invalid_arg "disj_union"

let list_disj_union = List.fold_left disj_union StringSet.empty

type isect = 
  {
    size : int;
    factor : float;
    is : StringSet.t;
    s1 : StringSet.t;
    s2 : StringSet.t;
    v1 : int;
    v2 : int;
  }

let make_factor i a b = 
  (float_of_int i) /. (sqrt (float_of_int (a*b)))

let build_isect s1 s2 v1 v2 = 
  let is = StringSet.inter s1 s2 in
  let size = StringSet.cardinal is in
  {
    size = size;
    factor = make_factor size (StringSet.cardinal s1) (StringSet.cardinal s2);
    is = is;
    s1 = s1;
    s2 = s2;
    v1 = v1;
    v2 = v2;
  } 

let summarize_isect is = 
  Printf.printf "%d\t%g\t%d\t%d\n" is.size is.factor is.v1 is.v2

module OrderedIsect = struct
  type t = isect
  let compare = Pervasives.compare
end

module IsectSet = Set.Make(OrderedIsect)

(* 
 * map from boot value to sets below.
 * note that we only store non-singletons.
 * *)
let ssim_of_tree t = 
  let m = ref IntMap.empty in
  let my_add k v = m := IntMap.add k v !m in
  let rec aux = function
    | Stree.Node(id, tL) ->
        let below = list_disj_union (List.map aux tL) in
        my_add (int_of_float (Gtree.get_boot t id)) below;
        below
    | Stree.Leaf(id) ->
        StringSet.singleton (Gtree.get_name t id)
  in
  let _ = aux (Gtree.get_stree t) in
  !m

let ssim1 = ssim_of_tree t1
let ssim2 = ssim_of_tree t2

let build_isects m1 m2 = 
  IntMap.fold
    (fun v1 s1 iss ->
      IntMap.fold
        (fun v2 s2 -> IsectSet.add (build_isect s1 s2 v1 v2))
        m2
        iss)
    m1
    IsectSet.empty

let overlap is1 is2 = 
  (is1.is = is2.is) || (is1.s1 = is2.s1) || (is1.s2 = is2.s2) 

let no_overlap is1 is2 = not (overlap is1 is2)

let pick iss = 
  let top = IsectSet.max_elt iss in
  (top, IsectSet.filter (no_overlap top) iss)

let pickn start_iss n = 
  let rec aux acc i iss = 
    if i=0 then List.rev acc 
    else begin
      let (top, rest) = pick iss in
      aux (top::acc) (i-1) rest
    end
  in
  aux [] n start_iss

let pick_all start_iss = 
  let rec aux acc iss = 
    if iss = IsectSet.empty then List.rev acc 
    else begin
        let (top, rest) = pick iss in
        aux (top::acc) rest
    end
  in
  aux [] start_iss

let summarize isl = 
  List.iter summarize_isect isl

let iss = build_isects ssim1 ssim2 
let alll = pick_all iss

let filter_by_factor cutoff isectl = 
  List.filter (fun isect -> isect.factor > cutoff) isectl

let filtered95 = filter_by_factor 0.95 alll

let () = summarize filtered95

let by_v1_map = List.fold_right (fun is -> IntMap.add is.v1 is) alll IntMap.empty

let g i = IntMap.find i by_v1_map

