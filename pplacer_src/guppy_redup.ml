open Subcommand
open Guppy_cmdobjs

let white_regexp = Str.regexp "[ \t]+"

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit placefile_cmd () as super_placefile

  val dupfile = flag "-d"
    (Needs_argument ("dupfile", "The dedup file to use to restore duplicates"))

  method specl = super_output#specl @ [string_flag dupfile]

  method desc = "restores duplicates to deduped placefiles"
  method usage = "usage: redup -d dupfile placefile"

  method private placefile_action = function
    | [pr] ->
      let lines = File_parsing.string_list_of_file (fv dupfile) in
      let sequence_tbl = Hashtbl.create 1024 in
      List.iter
        (fun line ->
          let sequences = Str.split white_regexp line in
          Hashtbl.add sequence_tbl (List.hd sequences) sequences)
        lines;
      let pr = Placerun.set_pqueries
        pr
        (List.map
           (fun pq ->
             let name = List.hd (Pquery.namel pq) in
             try
               Pquery.set_namel pq (Hashtbl.find sequence_tbl name)
             with
               | Not_found -> pq)
           (Placerun.get_pqueries pr))
      in
      Placerun_io.to_json_file
        "guppy to_json"
        (self#single_file ())
        pr

    | _ -> failwith "guppy redup takes exactly one placefile"
end
