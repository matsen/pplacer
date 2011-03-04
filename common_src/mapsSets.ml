open Ppr

module OrderedFloat = struct
  type t = float
  let compare = Pervasives.compare
end

module OrderedInt = struct
  type t = int
  let compare a b = a - b
end

module OrderedChar = struct
  type t = char
  let compare = Char.compare
end

module OrderedString = struct
  type t = string
  let compare = String.compare
end


(* *** Maps *** *)
module FloatMap = Map.Make(OrderedFloat)
module IntMap = Map.Make(OrderedInt)
module CharMap = Map.Make(OrderedChar)
module StringMap = Map.Make(OrderedString)


(* *** Sets *** *)
module FloatSet = Set.Make(OrderedFloat)
module IntSet = Set.Make(OrderedInt)
module CharSet = Set.Make(OrderedChar)
module StringSet = Set.Make(OrderedString)


module type STRINGABLE =
sig
  type t
  val to_string: t -> string
end

(* general things we might want to do with maps *)

module MapFuns (OT: Map.OrderedType) (SBLE: STRINGABLE with type t = OT.t) =
  struct
    module M = Map.Make(OT)

    let opt_add k optX map =
      match optX with
      | Some x -> M.add k x map
      | None -> map

    let opt_find k m = if M.mem k m then Some (M.find k m) else None

      (* if x is a key of m, make sure that it is bound to y.
       * otherwise, add (x,y) as a kv-pair to m *)
    let check_add x y m =
      if M.mem x m then
        if y = M.find x m then m else failwith "check_add"
      else
        M.add x y m

    let union m1 m2 =
      M.fold (
        fun k v m -> M.add k v m
    ) m1 m2

    let print val_to_string m =
      M.iter (
        fun k v ->
          Printf.printf "%s\t-> %s\n" (SBLE.to_string k) (val_to_string v)
          ) m

(* of_pairlist : given key, value pairs *)
    let rec of_pairlist = function
      | (k,v)::l -> M.add k v (of_pairlist l)
      | [] -> M.empty

(* listly means add values as a list associated with a key *)
    let add_listly k v m =
      if M.mem k m then M.add k (v::(M.find k m)) m
      else M.add k [v] m

(* the two given functions take the elements of the list into keys and values
 * for the map. these are then collected into a map from the key to the list of
 * all values associated with that key.
 * *)
    let of_f_list_listly ~key_f ~val_f pre_f_list =
      let rec aux m = function
        | x::l -> aux (add_listly (key_f x) (val_f x) m) l
        | [] -> m
      in
      aux M.empty (List.rev pre_f_list) (* the above rev's things *)

(* collect key,value pairs into a map from the key to the list of all values
 * associated with that key. *)
    let of_pairlist_listly l = of_f_list_listly ~key_f:fst ~val_f:snd l

    let nkeys m =
      M.fold (fun _ _ n -> n+1) m 0

  (* keys in increasing order *)
    let keys m =
      let l = M.fold (fun k _ l -> k::l) m [] in
      List.rev l

    let values m =
      let l = M.fold (fun _ v l -> v::l) m [] in
      List.rev l

    let to_pairs m =
      let l = M.fold (fun k v l -> (k,v)::l) m [] in
      List.rev l

    let ppr_gen ppr_val ff m =
      Format.fprintf ff "@[[";
      ppr_list_inners (
        fun ff (k, v) ->
          Format.fprintf ff "%s -> " (SBLE.to_string k);
          ppr_val ff v;
          ) ff (to_pairs m);
          Format.fprintf ff "]@]"

    let ppr_string = ppr_gen Format.pp_print_string
    let ppr_int = ppr_gen Format.pp_print_int
    let ppr_float = ppr_gen Format.pp_print_float
    let ppr_char = ppr_gen Format.pp_print_char
    let ppr_bool = ppr_gen Format.pp_print_bool
  end


module StringableFloat = struct
  type t = float
  let to_string = string_of_float
end

module StringableInt = struct
  type t = int
  let to_string = string_of_int
end

module StringableChar = struct
  type t = char
  let to_string c = Printf.sprintf "%c" c
  end

module StringableString = struct
  type t = string
  let to_string s = s
end


module FloatMapFuns = MapFuns (OrderedFloat) (StringableFloat)
module IntMapFuns = MapFuns (OrderedInt) (StringableInt)
module CharMapFuns = MapFuns (OrderedChar) (StringableChar)
module StringMapFuns = MapFuns (OrderedString) (StringableString)

(* general things we might want to do with sets *)

module SetFuns (OT: Map.OrderedType) (SBLE: STRINGABLE with type t = OT.t) =
  struct
    module S = Set.Make(OT)

    let of_list l = List.fold_right S.add l S.empty

    (* map from Set to Set of the same type. currying heaven. *)
    let map f s = S.fold (fun x -> S.add (f x)) s S.empty

    let ppr ff s =
      Format.fprintf ff "@[{";
      ppr_list_inners (
        fun ff x ->
          Format.fprintf ff "%s" (SBLE.to_string x);
          ) ff (S.elements s);
          Format.fprintf ff "}@]"
  end


module FloatSetFuns = SetFuns (OrderedFloat) (StringableFloat)
module IntSetFuns = SetFuns (OrderedInt) (StringableInt)
module CharSetFuns = SetFuns (OrderedChar) (StringableChar)
module StringSetFuns = SetFuns (OrderedString) (StringableString)
