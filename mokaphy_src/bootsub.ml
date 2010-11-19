
(* let x = boot_ssl  *)

let chosen_s = IntMap.find 407 ssim

let ss_of_list l = List.fold_right StringSet.add l StringSet.empty
let ss_of_ilist l = ss_of_list (List.map string_of_int l)

let symmdiff s s' = StringSet.union (StringSet.diff s s') (StringSet.diff s' s)

let x = ss_of_ilist [1;2;3]
let y = ss_of_ilist [3;4]

let r = symmdiff x y

(* f is a map from elements of list to some comparable quantities *)
let find_fsmallest f startl = 
  let rec aux x v = function
    | x'::l ->
        let v' = f x' in
        if v' < v then aux x' v' l
        else aux x v l
    | [] -> x
  in
  match startl with
  | [] -> assert false
  | h::t -> aux h (f h) t 

let ex_boot = List.hd boot_sssl 

(* given a StringSetSet, find the StringSet with the smallest symmetric
 * difference with chosen_s *)
let smallest_symmdiff strs sss = 
  find_fsmallest 
    StringSet.cardinal 
    (List.map (symmdiff strs) (StringSetSet.elements sss))

(* given a list of string sets, most_pop finds the element which is most
 * commonly seen *)
let most_pop ssl = 
  let h = Hashtbl.create ((StringSet.cardinal (List.hd ssl)) / 4) in
  let boost s = 
    if Hashtbl.mem h s then Hashtbl.replace h s (1 + (Hashtbl.find h s))
    else Hashtbl.add h s 1
  in
  List.iter (StringSet.iter boost) ssl;
  let (bk, bv) = 
    Hashtbl.fold 
      (fun k v ((bk, bv) as p) -> if v > bv then (k, v) else p) 
      h
      ("", -1)
  in
  assert(bv > -1);
  (bk, bv)

(* remove x from every elememnt of sss *)
let sssremove x sss = 
  StringSetSet.fold
    (fun ss -> 
        StringSetSet.add 
          (if StringSet.mem x ss then StringSet.remove x ss else ss))
    sss
    StringSetSet.empty

(* the score is how many of the SSets in our list will contain a set identical
 * to start_strs upon removal of the naughty elements *)
let perform cutoff start_strs start_sssl = 
  let rec aux strs sssl accu =
    let symdiffl = List.map (smallest_symmdiff strs) sssl in
    let (naughty,_) = most_pop symdiffl in
    let nsingle = StringSet.singleton naughty in
    let score = 
      List.fold_right 
        (fun s -> (+) (if s = StringSet.empty || s = nsingle then 1 else 0))
        symdiffl
        0
    in
    let accu' = (naughty, score)::accu in
    if score > cutoff then accu'
    else
      aux
        (StringSet.remove naughty strs)
        (List.map (sssremove naughty) sssl)
        accu'
  in
  aux start_strs start_sssl []

let x = perform 90 chosen_s boot_sssl

let remove_list = List.fold_right StringSet.remove 

let naughtyl = List.map fst x

let report = 
  (remove_list naughtyl chosen_s)

  (* let _ = Hashtbl.fold (fun k v l -> (k,v)::l) h [] in*)
