let empty_line_rex = Str.regexp "^[ \t]*$"
let comment_rex = Str.regexp "^[ \t]*#.*"

let filter_not some_rex string_list =
  List.filter
    (fun line -> not (Str.string_match some_rex line 0))
    string_list

let filter_empty_lines = filter_not empty_line_rex
let filter_comments = filter_not comment_rex

(* *** reading *** *)
let string_list_of_file fname =
  let ch = open_in fname in
  let rec get_line accu =
    try
      get_line ((input_line ch)::accu)
    with End_of_file -> close_in ch; List.rev accu
  in
  get_line []

(* read lines (at least one) until we hit a line which matches the given rex. if
  * the end of the file is reached, then raise End_of_file if no lines have
  * accumulated. otherwise return those lines *)
let read_lines_until ch rex =
  let rec get_line accu =
    try
      let line = input_line ch in
      if accu <> [] && Str.string_match rex line 0 then begin
        seek_in ch ((pos_in ch)-(String.length line)-1);
        List.rev accu
      end
      else get_line (line::accu)
    with End_of_file ->
      if accu = [] then raise End_of_file else List.rev accu
  in
  get_line []

(* partition_list:
  * splits up a list based on a predicate that signals the start of the list
# partition_list (fun x -> x = 2) [1;3;2;4;5;2;2;4];;
- : int list list = [[1; 3]; [2; 4; 5]; [2]; [2; 4]]
*)
let partition_list predicate start_l =
  let rec aux sublist_list current_sublist = function
    | x::l ->
        if predicate x then
          aux (current_sublist::sublist_list) [x] l
        else
          aux sublist_list (x::current_sublist) l
    | [] -> current_sublist::sublist_list
  in
  match start_l with
  | x::l -> List.rev (List.map List.rev (aux [] [x] l))
  | [] -> [[]]


(* find_beginning:
# find_beginning (fun x -> x = 2) [1;3;2;4;5;2;2;4];;
- : int * int list = (2, [4; 5; 2; 2; 4])
*)
let rec find_beginning predicate = function
  | x::l -> if predicate x then (x,l) else find_beginning predicate l
  | [] -> raise Not_found

(* separate_first_satisfactory:
# separate_first_satisfactory (fun x -> x = 2) [1;3;2;4;5;2;2;4];;
- : int list * int list = ([], [1; 3; 2; 4; 5; 2; 2; 4])
# separate_first_satisfactory (fun x -> x = 2) [2;2;3;2;4;5;2;2;4];;
- : int list * int list = ([2; 2], [3; 2; 4; 5; 2; 2; 4])
*)
let rec separate_first_satisfactory predicate start_l =
  let rec aux satisfactory = function
    | x::l as rest ->
        if predicate x then aux (x::satisfactory) l
        else (List.rev satisfactory, rest)
    | [] -> (List.rev satisfactory, [])
  in
  aux [] start_l

