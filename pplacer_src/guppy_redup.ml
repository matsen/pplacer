open Ppatteries
open Subcommand
open Guppy_cmdobjs

let white_regexp = Str.regexp "[ \t]+"

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output
  inherit placefile_cmd () as super_placefile

  val dupfile = flag "-d"
    (Needs_argument ("dupfile", "The dedup file to use to restore duplicates."))
  val multi_dupfile = flag "-m"
    (Plain (false, "If specified, redup with counts instead of a name list."))

  method specl = super_output#specl @ [
    string_flag dupfile;
    toggle_flag multi_dupfile;
  ]

  method desc = "restores duplicates to deduped placefiles"
  method usage = "usage: redup -d dupfile placefile"

  method private placefile_action = function
    | [pr] ->
      let pr' =
        if fv multi_dupfile then
          let sequence_tbl = Hashtbl.create 1024 in
          let f = function
            | [canon; rep; count] ->
              Hashtbl.add sequence_tbl canon (rep, float_of_string count)
            | _ -> failwith "malformed row in jplace"
          in
          fv dupfile
            |> open_in
            |> csv_in_channel
            |> Csv.of_in_obj
            |> Csv.iter ~f;
          Placerun.redup sequence_tbl pr
        else
          let sequence_tbl = Hashtbl.create 1024 in
          fv dupfile
            |> File.lines_of
            |> Enum.map (Str.split white_regexp |- (List.hd &&& identity))
            |> Enum.iter
                (fun (k, vs) ->
                  List.iter
                    (identity &&& const 1. |- Hashtbl.add sequence_tbl k)
                    vs);
          Placerun.redup sequence_tbl pr

      in
      self#write_placefile
        (self#single_file ())
        pr'

    | l ->
      List.length l
      |> Printf.sprintf "redup takes exactly one placefile (%d given)"
      |> failwith

end
