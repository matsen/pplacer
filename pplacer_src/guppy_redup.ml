open Subcommand
open Guppy_cmdobjs

let white_regexp = Str.regexp "[ \t]+"

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~show_fname:false () as super_output
  inherit placefile_cmd () as super_placefile

  val dupfile = flag "-d"
    (Needs_argument ("dupfile", "The dedup file to use to restore duplicates"))

  method specl = super_output#specl @ [string_flag dupfile]

  method desc = "restores duplicates to deduped placefiles"
  method usage = "usage: redup -d dupfile placefile[s]"

  method private placefile_action prl =
    let prefix = self#single_prefix in
    let lines = File_parsing.string_list_of_file (fv dupfile) in
    let sequence_tbl = Hashtbl.create 1024 in
    List.iter
      (fun line ->
        let sequences = Str.split white_regexp line in
        Hashtbl.add sequence_tbl (List.hd sequences) sequences)
      lines;
    List.iter
      (fun pr ->
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
        let out_name = (prefix ^ pr.Placerun.name ^ ".json") in
        Placerun_io.to_json_file
          "guppy to_json"
          out_name
          pr)
      prl
end
