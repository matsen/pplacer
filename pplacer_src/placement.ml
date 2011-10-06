(* * care and feeding of placements.
 *
 *)

open Ppatteries
open Stree

exception No_PP
exception No_classif
exception No_map_identity

let get_some except = function
  | Some pp -> pp
  | None -> raise except

type placement =
  {
    location: int;
    ml_ratio: float;
    post_prob: float option;
    log_like: float;
    marginal_prob: float option;
    distal_bl: float;
    pendant_bl: float;
    classif: Tax_id.tax_id option;
    map_identity: (float * int) option;
  }

let location            p = p.location
let ml_ratio            p = p.ml_ratio
let post_prob_opt       p = p.post_prob
let post_prob           p = get_some No_PP p.post_prob
let log_like            p = p.log_like
let marginal_prob_opt   p = p.marginal_prob
let marginal_prob       p = get_some No_PP p.marginal_prob
let distal_bl           p = p.distal_bl
let pendant_bl          p = p.pendant_bl
let classif_opt         p = p.classif
let classif             p = get_some No_classif p.classif
let map_identity_opt    p = p.map_identity
let map_identity        p = get_some No_map_identity p.map_identity

let make_ml loc ~ml_ratio ~log_like ~dist_bl ~pend_bl = {
  location         =  loc;
  ml_ratio         =  ml_ratio;
  log_like         =  log_like;
  distal_bl        =  dist_bl;
  pendant_bl       =  pend_bl;
  marginal_prob    =  None;
  post_prob        =  None;
  classif          =  None;
  map_identity     =  None;
}

let add_pp p ~marginal_prob ~post_prob =
  {p with
    post_prob      =  Some post_prob;
    marginal_prob  =  Some marginal_prob}

let add_classif p c = {p with classif = Some c}
let add_map_identity p i = {p with map_identity = Some i}

let sort_placecoll criterion pc =
  List.sort ~cmp:(comparing criterion |> flip) pc

let filter_place_list criterion cutoff pc =
  List.filter (fun p -> criterion p > cutoff) pc

let by_name_map_of_place_hash place_hash =
  Hashtbl.fold (
    fun _ (name, place) name_map ->
      if StringMap.mem name name_map then
        failwith(name^" appears multiply in name_map!")
      else
        StringMap.add name place name_map
  ) place_hash StringMap.empty

let make_ml_ratio_filter cutoff placement =
  placement.ml_ratio > cutoff

let make_post_prob_filter cutoff placement =
  match placement.post_prob with
  | Some x -> x > cutoff
  | None -> invalid_arg "make_post_prob_filter"


(* *** READING *** *)

let float_opt_of_string s =
  if s = "-" then None
  else Some (float_of_string s)

let taxid_opt_of_string s =
  if s = "-" then None
  else Some (Tax_id.of_old_string s)

(* we allow either 7 entry lines (no classification) or 9 entry lines *)
let placement_of_str str =
  let strs = Array.of_list (Str.split (Str.regexp "[ \t]+") str) in
  let len = Array.length strs in
  if len <> 7 && len <> 9 then
    failwith ("placement_of_str : wrong number of entries in "^str)
  else begin
    let basic =
      {
        location         =  int_of_string        strs.(0);
        ml_ratio         =  float_of_string      strs.(1);
        post_prob        =  float_opt_of_string  strs.(2);
        log_like         =  float_of_string      strs.(3);
        marginal_prob    =  float_opt_of_string  strs.(4);
        distal_bl        =  float_of_string      strs.(5);
        pendant_bl       =  float_of_string      strs.(6);
        classif          =  None;
        map_identity     =  None;
      }
    in
    if len = 7 then basic
    else
      {
        basic with
        classif  =  taxid_opt_of_string  strs.(7);
      }
  end


(* *** WRITING *** *)

(* usual output *)
let opt_to_str f = function
  | Some x -> f x
  | None -> "-"

let string_of_8gfloat = Printf.sprintf "%8g"
let string_of_gfloat = Printf.sprintf "%g"

let to_strl_gen fint ffloat ffloato ftaxido place =
  [
    fint place.location;
    ffloat place.ml_ratio;
    ffloato place.post_prob;
    ffloat place.log_like;
    ffloato place.marginal_prob;
    ffloat place.distal_bl;
    ffloat place.pendant_bl;
    ftaxido place.classif;
  ]

let to_strl =
  to_strl_gen
    string_of_int
    string_of_8gfloat
    (opt_to_str string_of_8gfloat)
    (opt_to_str Tax_id.to_string)

let to_str place = String.concat "\t" (to_strl place)

let to_json json_state place =
  begin match !json_state with
    | Some (has_post_prob, has_classif, has_map_identity) ->
      begin match place.post_prob, place.marginal_prob, has_post_prob with
        | Some _, Some _, true
        | None, None, false -> ()
        | _, _, _ -> failwith "not all placements have posterior probability"
      end;
      begin match place.classif, has_classif with
        | Some _, true
        | None, false -> ()
        | _, _ -> failwith "not all placements are classified"
      end;
      begin match place.map_identity, has_map_identity with
        | Some _, true
        | None, false -> ()
        | _, _ -> failwith "not all placement have MAP identity"
      end;
    | None ->
      let t = begin match place.post_prob, place.marginal_prob with
        | Some _, Some _ -> true
        | None, None -> false
        | _, _ ->
          failwith "placement has posterior probability but not marginal probability ???"
      end, begin match place.classif with
        | Some _ -> true
        | None -> false
      end, begin match place.map_identity with
        | Some _ -> true
        | None -> false
      end
      in
      json_state := Some t
  end;
  Jsontype.Array (
    [
      Jsontype.Int place.location;
      Jsontype.Float place.log_like;
      Jsontype.Float place.ml_ratio;
      Jsontype.Float place.distal_bl;
      Jsontype.Float place.pendant_bl;
    ]
    @ begin match place.post_prob, place.marginal_prob with
      | Some a, Some b -> [Jsontype.Float a; Jsontype.Float b]
      | _, _ -> []
    end
    @ begin match place.classif with
      | Some c -> [Tax_id.to_json c]
      | _ -> []
    end
    @ begin match place.map_identity with
      | Some (f, d) -> [Jsontype.Float f; Jsontype.Int d]
      | _ -> []
    end)

let of_json fields a =
  let a = Jsontype.array a in
  let map = List.fold_left2
    (fun m k v -> StringMap.add k v m)
    StringMap.empty
    fields
    a
  in
  let get k = StringMap.find k map
  and maybe_get f k =
    try
      Some (f (StringMap.find k map))
    with
      | Not_found -> None
  and map_identity = function
    | Jsontype.Array [Jsontype.Float f; Jsontype.Int d] -> f, d
    | _ -> failwith "malformed map_identity in json"
  in {
    location = Jsontype.int (get "edge_num");
    log_like = Jsontype.float (get "likelihood");
    ml_ratio = Jsontype.float (get "like_weight_ratio");
    distal_bl = Jsontype.float (get "distal_length");
    pendant_bl = Jsontype.float (get "pendant_length");
    post_prob = maybe_get Jsontype.float "post_prob";
    marginal_prob = maybe_get Jsontype.float "marginal_prob";
    classif = maybe_get Tax_id.of_json "classification";
    map_identity =
      match maybe_get identity "map_ratio",
        maybe_get identity "map_overlap"
      with
        | Some Jsontype.Float x, Some Jsontype.Int y -> Some (x, y)
        | None, None -> maybe_get map_identity "map_identity"
        | _, _ -> failwith "malformed map_identity in json";
  }

(* CSV *)
let opt_to_csv_str f = function
  | Some x -> f x
  | None -> "NA"

let to_csv_strl =
  to_strl_gen
    string_of_int
    string_of_gfloat
    (opt_to_csv_str string_of_gfloat)
    (opt_to_csv_str Tax_id.to_string)

