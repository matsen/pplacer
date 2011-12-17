open Ppatteries

(* bootstrap a list, with an option to modify the elements of the list through f
 * # boot_list (fun i x -> i+100*x) [1;2;3;4;5;6];;
 * - : int list = [102; 101; 201; 301; 501; 601]
 *
 * We check that the number of elements in the list is less than max_int, which is
 * 4,611,686,018,427,387,903 on a 64 bit machine.
 *
 * Run your Random.init before using this!
 *)
let boot_list f l =
  let n = List.length l in
  assert(n < max_int);
  let counts = Array.make n 0 in
  for i=1 to n do
    let draw = Random.int n in
    counts.(draw) <- counts.(draw) + 1
  done;
  let rec appendk acc elt k =
    if k>0 then appendk ((f k elt)::acc) elt (k-1)
    else acc
  in
  let rec aux i acc = function
    | hd::tl -> aux (i+1) (appendk acc hd counts.(i)) tl
    | [] -> acc
  in
  let out = aux 0 [] l in
  assert(n = List.length out);
  List.rev out

(* given an array of integers, resample from that array in proportion to the
 * number of entries in the array.
 * # multiplicity_boot rng [|1;10;100;1000;|];;
 * - : int array = [|1; 7; 91; 1012|]
*)
let multiplicity_boot rng int_arr =
  let total = Array.fold_left (+) 0 int_arr
  and disc = Gsl_randist.discrete_preproc (Array.map float_of_int int_arr)
  and out = Array.create (Array.length int_arr) 0
  in
  for i=1 to total do
    let picked = Gsl_randist.discrete rng disc in
    out.(picked) <- out.(picked) + 1
  done;
  out

(* stretch (by repeating) or shrink a list to a desired length
 * # Bootstrap.rubber_list [1;2;3] 8;;
 * - : int list = [1; 2; 3; 1; 2; 3; 1; 2]
 * # Bootstrap.rubber_list [1;2;3] 2;;
 * - : int list = [1; 2]
 * *)
let rubber_list l desired_len =
  let len = List.length l in
  let rec aux accu to_add =
    if to_add <= 0 then accu
    else if to_add >= len then aux (accu @ l) (to_add - len)
    else accu @ (ListFuns.sublist 0 (to_add-1) l)
  in
  aux [] desired_len

let boot_placerun rng pr =
  let pqa = Array.of_list (Placerun.get_pqueries pr) in
  let multa = multiplicity_boot
    rng
    (Array.map (Pquery.namlom |- List.length) pqa)
  and pql = ref []
  in
  let rubber_pquery pq desired_multi =
    assert(desired_multi > 0);
    Pquery.namlom pq
      |> flip rubber_list desired_multi
      |> Pquery.set_namlom pq
  in
  for i=(Array.length pqa)-1 downto 0 do
    if multa.(i) > 0 then
      pql := (rubber_pquery pqa.(i) multa.(i))::(!pql)
  done;
  let new_pr = { pr with Placerun.pqueries = !pql } in
  assert(Placerun.total_multiplicity pr = Placerun.total_multiplicity new_pr);
  new_pr

