(** A common base for code in pplacer. This module is designed to be opened.
    Opening it includes Batteries, MapsSets, and some generally useful
    functions. *)

include MapsSets
include Batteries

let round x = int_of_float (floor (x +. 0.5))

(*
  # int_pow 10. 3;;
  - : float = 1000.
  # int_pow 10. 0;;
  - : float = 1.
*)
let int_pow x n =
  assert(n >= 0);
  let rec aux accu i = if i=0 then accu else aux (x*.accu) (i-1) in
  aux 1. n

let string_of_fpclass = function
  | FP_normal ->    "FP_normal"
  | FP_subnormal -> "FP_subnormal"
  | FP_zero ->      "FP_zero"
  | FP_infinite ->  "FP_infinite"
  | FP_nan ->       "FP_nan"

let date_time_str () =
  let the_time = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d/%02d/%d %02d:%02d:%02d"
    (the_time.Unix.tm_mon+1)
    the_time.Unix.tm_mday
    (the_time.Unix.tm_year+1900)
    the_time.Unix.tm_hour
    the_time.Unix.tm_min
    the_time.Unix.tm_sec

let safe_chop_extension s =
  try Filename.chop_extension s with | Invalid_argument _ -> s

let safe_chop_suffix name suff =
  if Filename.check_suffix name suff then Filename.chop_suffix name suff
  else name

exception Different of int

let raise_if_different cmp x1 x2 =
  let c = cmp x1 x2 in
  if c <> 0 then raise (Different c)

let rec find_zero_pad_width n =
  assert(n>=0);
  if n <= 9 then 1
  else 1+(find_zero_pad_width (n/10))

let maybe_map_cons f = function
  | None -> identity
  | Some x -> f x |> List.cons
let maybe_cons o l = maybe_map_cons identity o l

let to_csv_out ch =
  (ch :> <close_out: unit -> unit; output: string -> int -> int -> int>)
let csv_out_channel ch = new IO.out_channel ch |> to_csv_out

let some x = Some x
let on f g a b = g (f a) (f b)
let comparing f a b = compare (f a) (f b)
let swap (a, b) = b, a
let junction pred f g a = if pred a then f a else g a
let (|--) f g a b = g (f a b)
let (|~) = (-|)
let (||-) f g a = f a || g a
let (||--) f g a b = f a b || g a b
let (&&-) f g a = f a && g a
let (&&--) f g a b = f a b && g a b

let approx_equal ?(epsilon = 1e-5) f1 f2 = abs_float (f1 -. f2) < epsilon
let (=~) = approx_equal

(* find the median of a sorted list. returns the left item if there are an even
 * number of items in the list. *)
let median l =
  let rec aux = function
    | e :: _, ([_] | [_; _]) -> e
    | _ :: tl1, _ :: _ :: tl2 -> aux (tl1, tl2)
    | _, _ -> invalid_arg "median"
  in
  aux (l, l)

let verbosity = ref 1
let dprintf ?(l = 1) ?(flush = true) fmt =
  if !verbosity >= l then begin
    finally (if flush then flush_all else identity) (Printf.printf fmt)
  end else
    Printf.ifprintf IO.stdnull fmt
let dprint ?(l = 1) ?(flush = true) s =
  if !verbosity >= l then begin
    print_string s;
    if flush then flush_all ();
  end

let align_with_space =
  List.map (Tuple3.map3 ((^) " ")) |- Arg.align

let get_dir_contents ?pred dir_name =
  let dirh = Unix.opendir dir_name in
  Enum.from
    (fun () ->
      try Unix.readdir dirh with End_of_file -> raise Enum.No_more_elements)
  |> Enum.suffix_action (fun () -> Unix.closedir dirh)
  |> Option.map_default Enum.filter identity pred
  |> Enum.map (Printf.sprintf "%s/%s" dir_name)

(*
 * 'a list MapsSets.IntMap.t list -> 'a list MapsSets.IntMap.t = <fun>
 * combine all the maps into a single one, with k bound to the concatenated set
 * of bindings for k in map_list.
 *)
let combine_list_intmaps l =
  (IntMap.fold
     (fun k -> IntMap.add_listly k |> flip |> List.fold_left |> flip)
   |> flip List.fold_left IntMap.empty)
    l

(* parsing *)
module Sparse = struct
  open Lexing

  let location_of_position p = p.pos_lnum, p.pos_cnum - p.pos_bol

  exception Parse_error of string * (int * int) * (int * int)
  let parse_error_of_positions s p1 p2 =
    Parse_error (s, location_of_position p1, location_of_position p2)

  let format_error = function
    | Parse_error (msg, (l1, c1), (l2, c2)) ->
      Printf.sprintf "%s between %d:%d and %d:%d" msg l1 c1 l2 c2
    | _ -> raise (Invalid_argument "format_error")

  let error_wrap f =
    try
      f ()
    with (Parse_error (_, _, _)) as e ->
      failwith (format_error e)

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  let syntax_error tok msg =
    raise (parse_error_of_positions msg (Parsing.rhs_start_pos tok) (Parsing.rhs_end_pos tok))

  let try_map f x tok msg =
    try
      f x
    with _ ->
      syntax_error tok msg

  exception Syntax_error of int * int

  let rec first_match groups s =
    match groups with
      | g :: rest ->
        begin
          try
            g, Str.matched_group g s
          with
            | Not_found -> first_match rest s
        end
      | [] -> raise Not_found

  let pos s ch =
    let rec aux pos line =
      try
        let pos' = String.index_from s pos '\n' in
        if pos' >= ch then
          line, ch - pos
        else
          aux (succ pos') (succ line)
      with
        | Not_found -> line, ch - pos
    in aux 0 1

  let tokenize_string regexp to_token ?eof_token s =
    let rec aux en accum =
      if String.length s = en then
        accum
      else if Str.string_match regexp s en then
        let accum =
          try
            (to_token s) :: accum
          with
            | Not_found -> accum
        in aux (Str.match_end ()) accum
      else
        let line, col = pos s en in
        raise (Syntax_error (line, col))
    in
    aux 0 []
      |> maybe_cons eof_token
      |> List.rev
      |> List.enum

  let gen_parsers tokenize parse =
    tokenize |- parse,
    File.lines_of |- Enum.map tokenize |- Enum.flatten |- parse

end

module ArrayFuns = struct

  (* find the first element of an array satisfying a predicate *)
  let first f a =
    let n = Array.length a in
    let rec aux i =
      if i >= n then invalid_arg "array_first: no first!"
      else if f (Array.unsafe_get a i) then i
      else aux (i+1)
    in
    aux 0

  (* find the last element of an array satisfying a predicate *)
  let last f a =
    let rec aux i =
      if i < 0 then invalid_arg "array_last: no last!"
      else if f (Array.unsafe_get a i) then i
      else aux (i-1)
    in
    aux ((Array.length a)-1)


  (* find the first instance of x in a *)
  let opt_find a to_find =
    let len = Array.length a in
    let rec aux pos =
      if pos >= len then None
      else if a.(pos) = to_find then Some pos
      else aux (pos+1)
    in
    aux 0

  let find a to_find =
    match opt_find a to_find with
      | Some x -> x
      | None -> raise Not_found

  let find_all_indices item arr =
    let found = ref [] in
    Array.iteri (
      fun i x ->
        if item = x then found := i::(!found)
    ) arr;
    List.rev !found

  let map2 f a b =
    let l = Array.length a in
    if l <> Array.length b then
      invalid_arg "map2: unequal length arrays";
    if l = 0 then [||] else begin
      let r = Array.create l
        (f (Array.unsafe_get a 0) (Array.unsafe_get b 0)) in
      for i = 1 to l - 1 do
        Array.unsafe_set r i
          (f (Array.unsafe_get a i) (Array.unsafe_get b i))
      done;
      r
    end

  let iter2 f a b =
    let l = Array.length a in
    if l <> Array.length b then
      invalid_arg "iter2: unequal length arrays";
    for i = 0 to l - 1 do
      f (Array.unsafe_get a i) (Array.unsafe_get b i)
    done

  let iter3 f a b c =
    let l = Array.length a in
    if l <> Array.length b || l <> Array.length c then
      invalid_arg "iter3: unequal length arrays";
    for i = 0 to l - 1 do
      f (Array.unsafe_get a i)
        (Array.unsafe_get b i)
        (Array.unsafe_get c i)
    done

  let iteri2 f a b =
    let l = Array.length a in
    if l <> Array.length b then
      invalid_arg "iteri2: unequal length arrays";
    for i = 0 to l - 1 do
      f i
        (Array.unsafe_get a i)
        (Array.unsafe_get b i)
    done

  let iteri3 f a b c =
    let l = Array.length a in
    if l <> Array.length b || l <> Array.length c then
      invalid_arg "iteri3: unequal length arrays";
    for i = 0 to l - 1 do
      f i
        (Array.unsafe_get a i)
        (Array.unsafe_get b i)
        (Array.unsafe_get c i)
    done

  let fold_left2 f x a b =
    let l = Array.length a in
    if l <> Array.length b then
      invalid_arg "fold_left2: unequal length arrays";
    let r = ref x in
    for i = 0 to l - 1 do
      r := f !r (Array.unsafe_get a i) (Array.unsafe_get b i)
    done;
    !r

  let fold_left3 f x a b c =
    let l = Array.length a in
    if l <> Array.length b || l <> Array.length c then
      invalid_arg "fold_left3: unequal length arrays";
    let r = ref x in
    for i = 0 to l - 1 do
      r := f !r
        (Array.unsafe_get a i)
        (Array.unsafe_get b i)
        (Array.unsafe_get c i)
    done;
    !r

  let fold_left4 f x a b c d =
    let l = Array.length a in
    if l <> Array.length b || l <> Array.length c || l <> Array.length d then
      invalid_arg "fold_left4: unequal length arrays";
    let r = ref x in
    for i = 0 to l - 1 do
      r := f !r
        (Array.unsafe_get a i)
        (Array.unsafe_get b i)
        (Array.unsafe_get c i)
        (Array.unsafe_get d i)
    done;
    !r

  let to_string entry_to_string a =
    "["^(
      String.concat "; " (
        List.map entry_to_string (
          Array.to_list a)))^"]"

end

module ListFuns = struct

  (* pull_each_out :
     # pull_each_out [1;2;3];;
     - : (int * int list) list = [(1, [2; 3]); (2, [1; 3]); (3, [1; 2])]
  *)
  let pull_each_out l =
    ((List.remove |- ((&&&) identity)) &&& identity |- uncurry List.map) l

  (* from a Pascal Cuoq post on stack overflow *)
  let rec sublist begini endi l =
    match l with
      | [] -> failwith "sublist"
      | h :: t ->
        let tail = if endi=0 then [] else sublist (begini-1) (endi-1) t in
        if begini>0 then tail else h :: tail

  let rec iter3 f l1 l2 l3 =
    match (l1, l2, l3) with
      | ([], [], []) -> ()
      | (a1::l1, a2::l2, a3::l3) -> f a1 a2 a3; iter3 f l1 l2 l3
      | (_, _, _) -> invalid_arg "ListFuns.iter3"

  let rec map3 f l1 l2 l3 =
    match (l1, l2, l3) with
        ([], [], []) -> []
      | (a1::l1, a2::l2, a3::l3) ->
        let r = f a1 a2 a3 in r :: map3 f l1 l2 l3
      | (_, _, _) -> invalid_arg "ListFuns.map3"

  let rec fold_left3 f accu l1 l2 l3 =
    match (l1, l2, l3) with
      | ([], [], []) -> accu
      | (a1::l1, a2::l2, a3::l3) -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
      | (_, _, _) -> invalid_arg "ListFuns.fold_left3"

  let partitioni p l =
    let rec part i yes no = function
      | [] -> (List.rev yes, List.rev no)
      | x :: l -> if p i x then part (i+1) (x :: yes) no l
        else part (i+1) yes (x :: no) l in
    part 0 [] [] l

  let rec remove_last = function
    | [] -> []
    | [_] -> []
    | h::t -> h::(remove_last t)

  let all_same = function
    | [] -> true
    | first::rest ->
      let rec aux = function
        | x::l -> (first = x) && (aux l)
        | [] -> true
      in
      aux rest

  (* multifilter: filter according to a list of criteria
     # let divbyk k x = x mod k = 0;;
     val divbyk : int -> int -> bool = <fun>
     # let x = multifilter [divbyk 2; divbyk 3] [1;2;3;4;5;6;7;8];;
     val x : int list list = [[2; 4; 6; 8]; [3; 6]]
  *)
  let multifilter f_list =
    let rec aux accu = function
      | [] -> List.map List.rev accu
      | x::l ->
        aux
          (List.map2
             (fun f category ->
               if f x then x::category else category)
             f_list
             accu)
          l
    in
    aux (List.map (fun _ -> []) f_list)


  (* iter over all ordered pairs in a list.
     # let print_pair = Printf.printf "(%d,%d) ";;
     val print_pair : int -> int -> unit = <fun>
     # list_iter_over_pairs_of_single print_pair [1;2;3;4];;
     (1,2) (1,3) (1,4) (2,3) (2,4) (3,4) - : unit = ()
  *)
  let rec list_iter_over_pairs_of_single f = function
    | x::l ->
      List.iter (fun y -> f x y) l;
      list_iter_over_pairs_of_single f l
    | [] -> ()

  let list_pairs_of_single l =
    let rec aux accum = function
      | x :: l ->
        aux
          (List.rev_append
             (List.map (fun y -> x, y) l)
             accum)
          l
      | [] -> accum
    in
    aux [] l

  (* iter over pairs from two lists.
     # list_iter_over_pairs_of_two print_pair [1;3] [4;5];;
     (1,4) (1,5) (3,4) (3,5) - : unit = ()
  *)
  let list_iter_over_pairs_of_two f l1 l2 =
    List.iter (fun x -> List.iter (f x) l2) l1

  (*
    # let divbyk k x = x mod k = 0;;
    val divbyk : int -> int -> bool = <fun>
    # let x = find_multiple_matches [divbyk 2; divbyk 3] [1;2;3;4;5;6;7;8];;
    val x : int list = [6]
  *)
  let find_multiple_matches f_list =
    let rec aux accu = function
      | [] -> List.rev accu
      | x::l ->
        if (List.fold_left
              (fun accu f ->
                accu+(if f x then 1 else 0))
              0
              f_list)
          > 1 then
          aux (x::accu) l
        else
          aux accu l
    in
    aux []

end

module MatrixFuns = struct

  let assert_rectangular m =
    if m <> [||] then begin
      let n_cols = Array.length m.(0) in
      for i=1 to (Array.length m)-1 do
        assert(n_cols = Array.length m.(i))
      done
    end

  let n_rows m = Array.length m
  let n_cols m = if m = [||] then 0 else Array.length m.(0)

  let map f m =
    Array.map (Array.map f) m

  let mapij f m =
    Array.mapi (fun i row -> Array.mapi (fun j x -> f i j x) row) m

  let iterij f m =
    Array.iteri (fun i row -> Array.iteri (fun j x -> f i j x) row) m

  let init n_rows n_cols f =
    Array.init
      n_rows
      (fun i -> Array.init n_cols (f i))

end

module StringFuns = struct

  let of_char_array a = Array.enum a |> String.of_enum

  let to_char_array s = String.enum s |> Array.of_enum

  let left_pad pad_width c s =
    assert(pad_width >= 0);
    let len = String.length s in
    let new_s = String.make (len+pad_width) c in
    String.blit s 0 new_s pad_width len;
    new_s

end

module EnumFuns = struct

  let n_cartesian_product ll =
    let pool = List.enum ll |> Enum.map Array.of_list |> Array.of_enum in
    let n = Array.length pool in
    let indices = Array.make n 0
    and lengths = Array.map (Array.length |- (+) (-1)) pool in
    let rec update_indices = function
      | i when indices.(i) <> lengths.(i) ->
        indices.(i) <- indices.(i) + 1
      | 0 -> raise Enum.No_more_elements
      | i ->
        indices.(i) <- 0; update_indices (i - 1)
    in
    let is_first = ref true in
    let next () =
      if !is_first then
        is_first := false
      else update_indices (n - 1);
      Array.map2 Array.get pool indices |> Array.to_list
    in
    Enum.from next

  let combinations l r =
    if r < 0 then
      invalid_arg "r must be non-negative";
    let pool = Array.of_list l
    and indices = Array.init r identity in
    let n = Array.length pool in
    let rec next_i = function
      | i when indices.(i) <> i + n - r -> i
      | 0 -> raise Enum.No_more_elements
      | i -> next_i (i - 1)
    in
    let is_first = ref true in
    let next () =
      if !is_first then
        is_first := false
      else begin
        let i = next_i (r - 1) in
        indices.(i) <- indices.(i) + 1;
        for j = i + 1 to r - 1 do indices.(j) <- indices.(j - 1) + 1 done
      end;
      Array.to_list indices |> List.map (Array.get pool)
    in
    Enum.from next

  let powerset l =
    1 -- List.length l
      |> Enum.map (combinations l)
      |> Enum.flatten

end

let () =
  Gsl_error.init ();
  Random.self_init ();
