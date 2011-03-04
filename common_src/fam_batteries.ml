(* 
LE MENU
-------
ArrayFuns
HashtblFuns
ListFuns
MatrixFuns
StringFuns
*)


(* *********** ArrayFuns *********** *)
module ArrayFuns = struct
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


(* *********** HashtblFuns *********** *)
module HashtblFuns = struct
  let keys h = Hashtbl.fold (fun k _ keys -> k::keys) h []
  let vals h = Hashtbl.fold (fun _ v keys -> v::keys) h []
  let to_pairs h =
    Hashtbl.fold (fun k v pairs -> (k, v)::pairs) h []
end


(* *********** ListFuns *********** *)
module ListFuns = struct

let range n =
  assert(n>=0);
  let rec aux i =
    if i>=n then []
    else i::(aux (i+1))
  in
  aux 0

let init n f =
  assert(n>=0);
  let rec aux i =
    if i>=n then []
    else (f i)::(aux (i+1))
  in
  aux 0

let iteri f l =
  let rec aux i = function
    | []   -> ()
    | a::l -> f i a; aux (i+1) l
  in
  aux 0 l

let mapi f l =
  let rec aux i = function
    | [] -> []
    | h::t -> let r = f i h in r :: aux (i+1) t
  in
  aux 0 l

(* from a Pascal Cuoq post on stack overflow *)
let rec sublist begini endi l =
  match l with
  | [] -> failwith "sublist"
  | h :: t ->
      let tail = if endi=0 then [] else sublist (begini-1) (endi-1) t in
      if begini>0 then tail else h :: tail

let rec iter2 f l1 l2 =
  match (l1, l2) with
  | ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "ListFuns.iter2"

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

let complete_fold_left f = function
 | hd::tl -> List.fold_left f hd tl
 | [] -> invalid_arg "complete_fold_left: given empty list!"

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

end


(* *********** MatrixFuns *********** *)
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



(* *********** StringFuns *********** *)
module StringFuns = struct
  let to_char_array s =
    let len = String.length s in
    let a = Array.make len 'x' in
    for i=0 to len-1 do
      a.(i) <- s.[i]
    done;
    a

  let of_char_array a =
    let s = String.make ( Array.length a ) ' ' in
    Array.iteri ( fun i c -> String.set s i c ) a;
    s

  let to_char_array s =
    let counter = ref 0 in
    let a = Array.make (String.length s) 'x' in
    String.iter (
      fun c ->
        a.(!counter) <- c;
        incr counter
    ) s;
    a

  let left_pad pad_width c s =
    assert(pad_width >= 0);
    let len = String.length s in
    let new_s = String.make (len+pad_width) c in
    String.blit s 0 new_s pad_width len;
    new_s

end
