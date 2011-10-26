open Batteries
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


module type PPRABLE =
sig
  type t
  val ppr: Format.formatter -> t -> unit
end

(* general things we might want to do with maps *)

module type M =
sig
  include Map.S

  val get: key -> 'a -> 'a t -> 'a
  val opt_add: key -> 'a option -> 'a t -> 'a t
  val opt_find: key -> 'a t -> 'a option
  val opt_extract: key -> 'a t -> 'a option * 'a t
  val check_add: key -> 'a -> 'a t -> 'a t
  val union: 'a t -> 'a t -> 'a t
  val of_pairlist: (key * 'a) list -> 'a t
  val add_listly: key -> 'a -> 'a list t -> 'a list t
  val of_f_list_listly: key_f:('a -> key) -> val_f:('a -> 'b) -> 'a list -> 'b list t
  val of_pairlist_listly: (key * 'a) list -> 'a list t
  val keylist: 'a t -> key list
  val to_pairs: 'a t -> (key * 'a) list
  val merge_counts: int t list -> int t
  val ppr_gen: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val ppr_string: Format.formatter -> string t -> unit
  val ppr_int: Format.formatter -> int t -> unit
  val ppr_float: Format.formatter -> float t -> unit
  val ppr_char: Format.formatter -> char t -> unit
  val ppr_bool: Format.formatter -> bool t -> unit
end

module BetterMap (OM: Map.S) (PBLE: PPRABLE with type t = OM.key) : (M with type key = OM.key) =
  struct
    include OM

    let get k default map =
      try
        find k map
      with
        | Not_found -> default

    let opt_add k optX map =
      match optX with
      | Some x -> add k x map
      | None -> map

    let opt_find k m = if mem k m then Some (find k m) else None

    let opt_extract k m =
      try
        let v, m' = extract k m in Some v, m'
      with Not_found -> None, m

      (* if x is a key of m, make sure that it is bound to y.
       * otherwise, add (x,y) as a kv-pair to m *)
    let check_add x y m =
      if mem x m then
        if y = find x m then m else failwith "check_add"
      else
        add x y m

    let union m1 m2 =
      fold (
        fun k v m -> add k v m
    ) m1 m2


(* of_pairlist : given key, value pairs *)
    let rec of_pairlist = function
      | (k,v)::l -> add k v (of_pairlist l)
      | [] -> empty

(* listly means add values as a list associated with a key *)
    let add_listly k v m =
      if mem k m then add k (v::(find k m)) m
      else add k [v] m

(* the two given functions take the elements of the list into keys and values
 * for the map. these are then collected into a map from the key to the list of
 * all values associated with that key.
 * *)
    let of_f_list_listly ~key_f ~val_f pre_f_list =
      let rec aux m = function
        | x::l -> aux (add_listly (key_f x) (val_f x) m) l
        | [] -> m
      in
      aux empty (List.rev pre_f_list) (* the above rev's things *)

(* collect key,value pairs into a map from the key to the list of all values
 * associated with that key. *)
    let of_pairlist_listly l = of_f_list_listly ~key_f:fst ~val_f:snd l

    let keylist m = keys m |> List.of_enum
    let to_pairs m = enum m |> List.of_enum

    let merge_counts ml =
      List.fold_left
        (fun accum m ->
          fold
            (fun k v m ->
              let cur_v =
                try
                  find k m
                with
                  | Not_found -> 0
              in
              add k (cur_v + v) m)
            m
            accum)
        empty
        ml

    let ppr_gen ppr_val ff m =
      Format.fprintf ff "@[[";
      ppr_list_inners (
        fun ff (k, v) ->
          Format.fprintf ff "%a -> " PBLE.ppr k;
          ppr_val ff v;
          ) ff (to_pairs m);
          Format.fprintf ff "]@]"

    (* Below: for ppr-ing maps with these value types. *)
    let ppr_string = ppr_gen Format.pp_print_string
    let ppr_int = ppr_gen Format.pp_print_int
    let ppr_float = ppr_gen Format.pp_print_float
    let ppr_char = ppr_gen Format.pp_print_char
    let ppr_bool = ppr_gen Format.pp_print_bool
  end


module PprFloat = struct
  type t = float
  let ppr = Format.pp_print_float
end

module PprInt = struct
  type t = int
  let ppr = Format.pp_print_int
end

module PprChar = struct
  type t = char
  let ppr = Format.pp_print_char
  end

module PprString = struct
  type t = string
  let ppr = Format.pp_print_string
end


module FloatMap = BetterMap (Map.Make(OrderedFloat)) (PprFloat)
module IntMap = BetterMap (Map.Make(OrderedInt)) (PprInt)
module CharMap = BetterMap (Map.Make(OrderedChar)) (PprChar)
module StringMap = BetterMap (Map.Make(OrderedString)) (PprString)

(* general things we might want to do with sets *)

module type S =
sig
  include Set.S
  val of_list: elt list -> t
  val ppr: Format.formatter -> t -> unit
end

module BetterSet (OS: Set.S) (PBLE: PPRABLE with type t = OS.elt) : (S with type elt = OS.elt) =
  struct
    include OS

    let of_list l = List.fold_left (flip add) empty l

    let ppr ff s =
      Format.fprintf ff "@[{";
      ppr_list_inners (
        fun ff x ->
          Format.fprintf ff "%a" PBLE.ppr x;
          ) ff (elements s);
          Format.fprintf ff "}@]"
  end


module FloatSet = BetterSet (Set.Make(OrderedFloat)) (PprFloat)
module IntSet = BetterSet (Set.Make(OrderedInt)) (PprInt)
module CharSet = BetterSet (Set.Make(OrderedChar)) (PprChar)
module StringSet = BetterSet (Set.Make(OrderedString)) (PprString)
