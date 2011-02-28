open MapsSets


let refpkg_str = "CONTENTS.json"

let sstringMap_of_Sjobj o =
  Hashtbl.fold (fun k v -> StringMap.add k (Simple_json.get_string v))
               (Simple_json.get_hashtbl o)
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
  let contents = Simple_json.of_file (dirize refpkg_str)
  in
  StringMap.add
    "name"
    (Base.safe_chop_suffix (Filename.basename noslash) ".refpkg")
    (StringMap.map dirize
      (sstringMap_of_Sjobj (Simple_json.find contents "files")))
