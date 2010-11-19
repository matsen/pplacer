
module Bootsub 
         (O: Set.OrderedType) 
         (S: Set.S with type elt = O.t)
         (SS: Set.S with type elt = S.t) = struct

  (* madness *)
  exception First of O.t
  let first_key h = 
    try Hashtbl.iter (fun k _ -> raise (First k)) h; raise Not_found with 
    | First k -> k

  let s_of_list l = List.fold_right S.add l S.empty

  (*
   * # (x,y);;
  * - : MapsSets.StringSet.t * MapsSets.StringSet.t = ({1; 2; 3}, {3; 4})
  * # let r = symmdiff x y;;
  * val r : MapsSets.StringSet.t = {1; 2; 4}
  *)
  let symmdiff s s' = S.union (S.diff s s') (S.diff s' s)

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
  
  (* given a StringSetSet, find the StringSet with the smallest symmetric
   * difference with chosen_s *)
  let smallest_symmdiff s ss = 
    find_fsmallest S.cardinal (List.map (symmdiff s) (SS.elements ss))
  
  (* given a list of string sets, most_pop finds the element which is most
   * commonly seen *)
  let most_pop sl = 
    let h = Hashtbl.create ((S.cardinal (List.hd sl)) / 4) in
    let boost x = 
      if Hashtbl.mem h x then Hashtbl.replace h x (1 + (Hashtbl.find h x))
      else Hashtbl.add h x 1
    in
    List.iter (S.iter boost) sl;
    let fk = first_key h in
    let (bk, bv) = 
      Hashtbl.fold 
        (fun k v ((bk, bv) as p) -> if v > bv then (k, v) else p) 
        h
        (fk, Hashtbl.find h fk)
    in
    (bk, bv)
  
  (* remove x from every elememnt of ss *)
  let ssremove x ss = 
    SS.fold
      (fun s -> SS.add (if S.mem x s then S.remove x s else s)) ss SS.empty
  
  (* the score is how many of the SSets in our list will contain a set identical
   * to start_s upon removal of the naughty elements *)
  let perform cutoff start_s start_ssl = 
    let rec aux s ssl accu =
      let symdiffl = List.map (smallest_symmdiff s) ssl in
      let score = 
        List.fold_right 
          (fun s -> (+) (if s = S.empty then 1 else 0))
          symdiffl
          0
      in
      if score >= cutoff then accu
      else begin
        let (naughty,_) = most_pop symdiffl in
          aux
            (S.remove naughty s)
            (List.map (ssremove naughty) ssl)
            ((naughty, score)::accu)
      end
    in
    aux start_s start_ssl []

end

module StrBootsub = 
  Bootsub (MapsSets.OrderedString) (MapsSets.StringSet) (StringSetSet)

let my_perform i = StrBootsub.perform 95 (IntMap.find i ssim) boot_sssl

let no_p2z1r79 = List.map (StrBootsub.ssremove "p2z1r79") boot_sssl

let no_perform i = 
  StrBootsub.perform 95 (StringSet.remove "p2z1r79" (IntMap.find i ssim))
  no_p2z1r79

let go i = (i, (IntMap.find i ssim, no_perform i))

  (*
let _ = 
  List.map build
  [
    413;
    327;
    433;
    388;
    370;
    424;
    386;
    356;
    418;
    430;
    331;
    389;
    387;
    308;
    412;
    309;
    314;
    322;
  ]

let remove_list = List.fold_right StringSet.remove 

let naughtyl = List.map fst x

let report = 
  (remove_list naughtyl chosen_s)

  (* let _ = Hashtbl.fold (fun k v l -> (k,v)::l) h [] in*)
*)


