open Ppatteries
open Rst

let docs_dir = "docs/"
let details_dir = docs_dir^"details/"
let generated_dir = docs_dir^"generated_rst/"

let check_directory path =
  if not (Sys.is_directory path) then
    failwith
      ("Could not find the "^path^" directory. \
      Please run this command in the pplacer repository root directory.")

(* copy the contents of fname into the channel *)
let cat_file_to_channel fname out_ch =
  let in_ch = open_in fname in
  try
    while true; do Printf.fprintf out_ch "%s\n" (input_line in_ch) done
  with End_of_file -> close_in in_ch


let () =

  List.iter check_directory [docs_dir; details_dir; generated_dir;];

  let guppy_commands = Guppy_commands.command_list ()
    |> List.map snd
    |> List.flatten
  in
  let command_matrix =
    Array.append
      [|[|"Command"; "Description"|]|]
      (Array.of_list
         (List.sort
            (List.map
               (fun (name, cmd) ->
                 [|Printf.sprintf ":ref:`%s <guppy_%s>`" name name;
                   (cmd ())#desc|])
               guppy_commands)))
  in
  let guppy_out = generated_dir ^ "guppy.rst" |> open_out in
  details_dir ^ "guppy.rst"
    |> File.lines_of
    |> Enum.iter
        (fun line ->
          if line = ".. guppy-command-table" then
            Rst.write_table guppy_out command_matrix
          else
            Printf.fprintf guppy_out "%s\n" line);
  close_out guppy_out;

  List.iter
    (fun (name, prefix, pre_o) ->
      let o = pre_o ()
      and full_name =
        (match prefix with
          | None -> ""
          | Some s -> s ^ "_")
        ^ name
      in
      let rst_name = full_name ^ ".rst" in
      let ch = open_out (generated_dir^rst_name) in
      let endline () = Printf.fprintf ch "\n" in

      (* write top matter *)
      Printf.fprintf ch ":tocdepth: 3\n\n";
      Printf.fprintf ch ".. _%s:\n\n" full_name;
      write_top_title ch name;
      Printf.fprintf ch "\n`%s` %s." name o#desc;
      endline ();
      write_literal_block_start ch;
      Printf.fprintf ch "  %s\n\n" o#usage;

      (* write flags *)
      write_section_title ch "Options" 0;
      endline ();
      List.iter
        (fun (flagstr,_,description) -> write_option ch flagstr description)
        o#specl;
      endline ();

      (* write details if they exist *)
      let details_path = details_dir^rst_name in
      if Sys.file_exists details_path then begin
        write_section_title ch "Details" 0;
        endline ();
        cat_file_to_channel details_path ch;
      end;

      print_endline ("Wrote "^generated_dir^rst_name^"...")

    )
    (("pplacer", None, (fun () -> new Prefs.pplacer_cmd ()))::
        (List.map
           (fun (name, pre_o) -> name, Some "guppy", pre_o)
           guppy_commands))
