open MapsSets


let refpkg_str = "CONTENTS.json"

let sstringMap_of_Sjobj obj =
  Hashtbl.fold
    (fun k v -> StringMap.add k (Simple_json.get_string v))
    obj
    StringMap.empty

(* *** parsing *** *)

let remove_terminal_slash s =
  let len = String.length s in
  if s.[len - 1] <> '/' then s
  else String.sub s 0 (len-1)

let strmap_of_path path =
  if not (Sys.is_directory path) then
    failwith ("Purported refpkg "^path^" is not a directory");
  let noslash = remove_terminal_slash path in
  let dirize fname = noslash^"/"^fname in
  let contents = Jsontype.obj (Json.of_file (dirize refpkg_str)) in
  let map = StringMap.map dirize
    (sstringMap_of_Sjobj (Jsontype.obj (Hashtbl.find contents "files")))
  in
  let map' =
    try
      StringMap.add
        "format_version"
        (Jsontype.string
           (Hashtbl.find
              (Jsontype.obj (Hashtbl.find contents "metadata"))
              "format_version"))
        map
    with Not_found ->
      map
  in
  StringMap.add
    "name"
    (Base.safe_chop_suffix (Filename.basename noslash) ".refpkg")
    map'
