(** A common base for code in pplacer. This module is designed to be opened.
    Opening it includes Batteries, MapsSets, and some generally useful
    functions. *)

include MapsSets
include AlgMap
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
  (ch :> <close_out: unit -> unit; output: bytes -> int -> int -> int>)
let csv_out_channel ch = new IO.out_channel ch |> to_csv_out

let to_csv_in ch = object
  method input a b c =
    try
      ch#input a b c
    with IO.No_more_input ->
      raise End_of_file
  method close_in = ch#close_in
end
let csv_in_channel ch = new IO.in_channel ch |> to_csv_in

(*let first = Tuple.Tuple2.map1*)
(*let second = Tuple.Tuple2.map2*)

let some x = Some x
let on f g a b = g (f a) (f b)
let comparing f a b = compare (f a) (f b)
let swap (a, b) = b, a
let junction pred f g a = if pred a then f a else g a
let fold_both f g a (x, y) = f a x, g a y
(* Replaced by '%>' in batteries 2.0 final.
 * Maintained to preserve operator precedence in existing code *)
let (|-) f g x = g (f x)
let (|--) f g a b = g (f a b)
let (|~) = (%)
let (||-) f g a = f a || g a
let (||--) f g a b = f a b || g a b
let ( &&& ) f g = fun x -> (f x, g x) (* removed in batteries 2.0 final *)
let (&&-) f g a = f a && g a
let (&&--) f g a b = f a b && g a b

let approx_equal ?(epsilon = 1e-5) f1 f2 = abs_float (f1 -. f2) < epsilon
let (=~) = approx_equal
let approx_compare ?epsilon f1 f2 =
  if approx_equal ?epsilon f1 f2 then 0 else compare f1 f2
let (<~>) = approx_compare

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
let dfprintf ?(l = 1) ?(flush = true) ch fmt =
  if !verbosity >= l then begin
    finally (if flush then flush_all else identity) (Printf.fprintf ch fmt)
  end else
    Printf.ifprintf IO.stdnull fmt
let dprintf ?l ?flush = dfprintf ?l ?flush stdout
let deprintf ?l ?flush = dfprintf ?l ?flush stderr
let dfprint ?l ?flush ch s = dfprintf ?l ?flush ch "%s" s
let dprint ?l ?flush = dfprint ?l ?flush stdout
let deprint ?l ?flush = dfprint ?l ?flush stderr

let align_with_space =
  List.map (Tuple3.map3 ((^) " ")) %> Arg.align

let progress_displayer ?(update_interval = 0.3) fmt total =
  let shown = ref 0
  and last_length = ref 0
  and last_time = ref 0. in
  if Unix.isatty Unix.stdout then begin fun name ->
    incr shown;
    let shown = !shown
    and time = Unix.gettimeofday () in
    if time -. !last_time > update_interval || shown = total then begin
      let msg = Printf.sprintf fmt name shown total in
      let msg_len = String.length msg in
      let padding = String.make (!last_length - msg_len |> max 0) ' ' in
      dprintf "%s%s\r" msg padding;
      last_length := msg_len;
      last_time := time;
    end;
    if shown = total then dprint "\n";
  end else begin fun name ->
    incr shown;
    Printf.sprintf fmt name !shown total |> dprintf "%s\n";
  end


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

(* ll_normalized_prob :
 * ll_list is a list of log likelihoods. this function gives the normalized
 * probabilities, i.e. exponentiate then our_like / (sum other_likes)
 * have to do it this way to avoid underflow problems.
 * *)
let ll_normalized_prob ll_list =
  List.map
    (fun log_like ->
      1. /.
        (List.fold_left ( +. ) 0.
          (List.map
            (fun other_ll -> exp (other_ll -. log_like))
            ll_list)))
    ll_list

(* Functions here mimic BatFile and Filename,
 * but support decompression of gzipped files (ending in .gz).
 * All Filename like functions operate on filenames with a .gz extension
 * dropped, so chop_extension "test.fasta.gz" == "test" *)
module MaybeZipped = struct
  let is_gzipped name =
    Filename.check_suffix name ".gz"

  let drop_gz name =
    if is_gzipped name then
      Filename.chop_extension name
    else
      name

  let check_suffix name suffix =
    Filename.check_suffix (drop_gz name) suffix

  let chop_suffix name suffix =
    Filename.chop_suffix (drop_gz name) suffix

  let chop_extension name =
    Filename.chop_extension (drop_gz name)

  (* Mostly duplicates above. *)
  let safe_chop_extension s =
    let s = drop_gz s in
    try chop_extension (drop_gz s) with | Invalid_argument _ -> s

  let safe_chop_suffix name suff =
    let name = drop_gz name in
    if check_suffix name suff then chop_suffix name suff
    else name

  let open_in name =
    if is_gzipped name then
      let in_chan = Gzip.open_in name in
      IO.create_in
        ~read:(fun () ->
          try
            Gzip.input_char in_chan
          with End_of_file -> raise BatIO.No_more_input)
        ~input:(fun buf pos len ->
          try
            Gzip.input in_chan buf pos len
          with End_of_file -> raise BatIO.No_more_input)
        ~close:(fun () -> Gzip.close_in in_chan)
    else
      File.open_in name

  let lines_of name =
    open_in name |> IO.lines_of

  let open_out name =
    if is_gzipped name then
      let out_chan = Gzip.open_out name in
      (* Gzip.output doesn't return the number of characters written,
       * hence the Buffer wrapping *)
      IO.create_out
        ~write:(Gzip.output_char out_chan)
        ~output:(fun s p l ->
          let buf = Buffer.create l in
          Buffer.add_subbytes buf s p l;
          Gzip.output out_chan (Buffer.to_bytes buf) 0 (Buffer.length buf);
          Buffer.length buf)
        ~close:(fun () -> Gzip.close_out out_chan)
        (* Gzip.flush disposes of the Gzip stream and prevents further writes -
         * we don't want that, so no action here. *)
        ~flush:(fun () -> ())
    else
      File.open_out name
end

(* parsing *)
module Sparse = struct
  open Lexing

  let update_fref file fref =
    match !fref with
      | None -> fref := Some file
      | Some _ -> ()

  let format_fref ?(prep = "of") fref =
    Option.map_default (Printf.sprintf " %s %s" prep) "" !fref

  let location_of_position p = p.pos_lnum, p.pos_cnum - p.pos_bol

  exception Parse_error of string * (int * int) * (int * int) * string option ref
  let parse_error_of_positions ?file s p1 p2 =
    Parse_error (s, location_of_position p1, location_of_position p2, ref file)

  let () =
    Printexc.register_printer
      (function
        | Parse_error (msg, (l1, c1), (l2, c2), file) ->
          Some (Printf.sprintf
                  "%s between line %d character %d and line %d character %d%s"
                  msg
                  l1
                  c1
                  l2
                  c2
                  (format_fref file))
        | _ -> None)

  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  let parse_error ?file tok msg =
    raise
      (parse_error_of_positions ?file msg (Parsing.rhs_start_pos tok) (Parsing.rhs_end_pos tok))

  let try_map f x tok msg =
    try
      f x
    with _ ->
      parse_error tok msg

  exception Tokenizer_error of int * int * string option ref
  exception Syntax_error of string * string option ref

  let syntax_error msg =
    raise (Syntax_error (msg, ref None))

  let () =
    Printexc.register_printer
      (function
        | Tokenizer_error (line, col, file) ->
          Some (Printf.sprintf
                  "syntax error at line %d character %d%s"
                  line
                  col
                  (format_fref file))
        | Syntax_error (msg, file) ->
          Some (Printf.sprintf
                  "syntax error%s: %s"
                  (format_fref ~prep:"in" file)
                  msg)
        | _ -> None)

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
        raise (Tokenizer_error (line, col, ref None))
    in
    aux 0 []
      |> maybe_cons eof_token
      |> List.rev
      |> List.enum

  let file_parse_wrap file f x =
    try
      f x
    with
      | Tokenizer_error (_, _, fref)
      | Syntax_error (_, fref)
      | Parse_error (_, _, _, fref) as e ->
        update_fref file fref;
        raise e

  let wrap_of_fname_opt = function
    | None -> identity
    | Some fname -> file_parse_wrap fname

  let gen_parsers tokenize parse =
    let of_string ?fname s =
      wrap_of_fname_opt fname (tokenize %> parse) s
    and of_file fname =
      file_parse_wrap
        fname
        (MaybeZipped.lines_of %> Enum.map tokenize %> Enum.flatten %> parse)
        fname
    in
    of_string, of_file

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
      let r = Array.make l
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
  let pull_each_out init_list =
    let rec aux all_pairs start = function
      | x::l ->
          aux ((x, List.rev_append start l)::all_pairs) (x::start) l
      | [] -> all_pairs
    in
    List.rev (aux [] [] init_list)

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
    let new_s = Bytes.make (len+pad_width) c in
    String.blit s 0 new_s pad_width len;
    Bytes.unsafe_to_string new_s

end

module EnumFuns = struct

  let n_cartesian_product ll =
    if List.is_empty ll then
      invalid_arg "n_cartesian_product: list-list empty";
    if List.exists List.is_empty ll then
      invalid_arg "n_cartesian_procuct: some list empty";
    let pool = List.enum ll |> Enum.map Array.of_list |> Array.of_enum in
    let n = Array.length pool in
    let indices = Array.make n 0
    and lengths = Array.map (Array.length %> (+) (-1)) pool in
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
    if n < r then Enum.empty () else Enum.from next

  let powerset l =
    1 -- List.length l
      |> Enum.map (combinations l)
      |> Enum.flatten

end

let exn_wrap f = Printexc.pass f ()

let () =
  Gsl.Error.init ();
  Printexc.register_printer
    (function
     | Unix.Unix_error (errno, fn, param) ->
       let op = if param = "" then "" else Printf.sprintf " on %S" param in
       Some
         (Printf.sprintf "error %S from %s%s"
            (Unix.error_message errno)
            fn
            op)
     | _ -> None)

let memory_stats_ch =
  match begin
    try Some (Sys.getenv "PPLACER_MEMORY_STATS")
    with Not_found -> None
  end with
  | None -> None
  | Some statsfile ->
    let ch = Legacy.open_out_gen
      [Open_append; Open_creat; Open_trunc]
      0o600
      statsfile
    in
    let write = Csv.to_channel ch |> Csv.output_record in
    let word_in_bytes = Sys.word_size / 8 in
    let start = Unix.gettimeofday () in
    let last_top = ref None in
    write ["time"; "pid"; "top_heap_bytes"];
    let check_stats () =
      let stats = Gc.quick_stat () in
      match !last_top with
      | Some top when stats.Gc.top_heap_words <= top -> ()
      | _ ->
        write
          [Printf.sprintf "%f" (Unix.gettimeofday () -. start);
           string_of_int (Unix.getpid ());
           string_of_int (stats.Gc.top_heap_words * word_in_bytes)];
        Legacy.flush ch;
        last_top := Some stats.Gc.top_heap_words
    in
    let _ = Gc.create_alarm check_stats in
    at_exit check_stats;
    Some ch
