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

let command_list_of_subcommand_program (prefix, command_list) =
  let commands = Base.map_and_flatten snd command_list in
  let command_matrix =
    Array.append
      [|[|"Command"; "Description"|]|]
      (Array.of_list
         (List.sort
            compare
            (List.map
               (fun (name, cmd) ->
                 [|Printf.sprintf ":ref:`%s <%s_%s>`" name prefix name;
                   (cmd ())#desc|])
               commands)))
  in
  let index_out = open_out (Printf.sprintf "%s%s.rst" generated_dir prefix) in
  List.iter
    (fun line ->
      if line = ".. command-table" then
        Rst.write_table index_out command_matrix
      else
        Printf.fprintf index_out "%s\n" line)
    (File_parsing.string_list_of_file (Printf.sprintf "%s%s.rst" details_dir prefix));
  close_out index_out;
  List.map
    (fun (name, pre_o) -> name, Some prefix, pre_o)
    commands


let () =

  List.iter check_directory [docs_dir; details_dir; generated_dir;];

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
    (("pplacer", None, (fun () -> new Prefs.pplacer_cmd ()))
     :: (Base.map_and_flatten command_list_of_subcommand_program
           ["guppy", Guppy.command_list ();
            "rppr", Rppr.command_list ()]))

