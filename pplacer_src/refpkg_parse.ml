open Ppatteries


let refpkg_str = "CONTENTS.json"

let sstringMap_of_Sjobj obj =
  Hashtbl.fold
    (fun k v -> StringMap.add k (Simple_json.get_string v))
    obj
    StringMap.empty

(* *** parsing *** *)

type contents =
  | File_path of string
  | File_contents of string * string Lazy.t
  | Metadata of string

let file_path s = File_path s
let metadata s = Metadata s

let as_metadata = function
  | Metadata s -> s
  | _ -> invalid_arg "as_metadata"

let as_file_path = function
  | File_path p -> p
  | _ -> invalid_arg "as_file_path"

let of_file_or_string of_file (of_string: ?fname:string -> string -> 'a) = function
  | File_path p -> of_file p
  | File_contents (fname, s) -> of_string ~fname (Lazy.force s)
  | _ -> invalid_arg "of_file_or_string"

let json_of_contents = of_file_or_string Json.of_file Json.of_string
let csv_of_contents c =
  (match c with
    | File_path p -> open_in p
    | File_contents (_, s) -> IO.input_string (Lazy.force s)
    | _ -> invalid_arg "csv_of_contents")
  |> csv_in_channel
  |> Csv.of_in_obj

let remove_terminal_slash s =
  let len = String.length s in
  if s.[len - 1] <> '/' then s
  else String.sub s 0 (len-1)

let maybe_add_version contents map =
  try
    StringMap.add
      "format_version"
      (Jsontype.string
         (Hashtbl.find
            (Jsontype.obj (Hashtbl.find contents "metadata"))
            "format_version")
       |> metadata)
      map
  with Not_found ->
    map

let strmap_of_zipfile path =
  let z = Zip.open_in path in
  let dirent = match List.filter (fun e -> e.Zip.is_directory) (Zip.entries z) with
  | [e] -> e
  | _ -> failwith "reference package zipfiles must contain exactly one directory"
  in
  let dirname = dirent.Zip.filename in
  let fname = dirname ^ refpkg_str in
  let contents = fname
    |> Zip.find_entry z
    |> Zip.read_entry z
    |> Json.of_string ~fname
    |> Jsontype.obj
  and read_contents ent =
    File_contents (ent.Zip.filename, (lazy (Zip.read_entry z ent)))
  in
  Hashtbl.find contents "files"
    |> Jsontype.obj
    |> sstringMap_of_Sjobj
    |> StringMap.map ((^) dirname |- Zip.find_entry z |- read_contents)
    |> maybe_add_version contents
    |> StringMap.add
        "name"
        (safe_chop_suffix (Filename.basename path) ".refpkg" |> metadata)

let strmap_of_dir path =
  let noslash = remove_terminal_slash path in
  let dirize fname = noslash^"/"^fname in
  let contents = Jsontype.obj (Json.of_file (dirize refpkg_str)) in
  Hashtbl.find contents "files"
    |> Jsontype.obj
    |> sstringMap_of_Sjobj
    |> StringMap.map (dirize |- file_path)
    |> maybe_add_version contents
    |> StringMap.add
        "name"
        (safe_chop_suffix (Filename.basename noslash) ".refpkg" |> metadata)

let strmap_of_path path =
  if Sys.is_directory path then
    strmap_of_dir path
  else
    strmap_of_zipfile path
