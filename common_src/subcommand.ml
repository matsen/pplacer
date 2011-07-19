(* A specl is a specification list, which gets passed to Arg.parse_argv or
 * wrap_parse_argv. It specifies the options and the actions which are assocated
 * with those options.
*)

open MapsSets

let option_rex = Str.regexp "-.*"

(* print the commands available through cmd_map *)
let print_avail_cmds prg_name (display_map, longest) =
  print_endline "Here is a list of commands available using this interface:";
  List.iter
    (fun (name, map) ->
      Printf.printf "  %s\n" name;
      StringMap.iter
        (fun k v -> Printf.printf "    %-*s  %s\n" longest k (v ())#desc) map;
      Printf.printf "\n"
    )
    display_map;
  Printf.printf
    "To get more help about a given command, type %s COMMAND --help\n"
    prg_name;
  ()

(* given an argl, process a subcommand *)
let process_cmd prg_name display_map cmd_map argl =
  let print_need_cmd_error () =
    Printf.printf
      "please specify a %s command, e.g. %s COMMAND [...]"
      prg_name prg_name;
    print_avail_cmds prg_name display_map;
    exit 1
  in
  match argl with
    | s::_ ->
      if StringMap.mem s cmd_map then
        ((StringMap.find s cmd_map) ())#run argl
      else if Str.string_match option_rex s 0 then
        print_need_cmd_error ()
      else begin
        print_endline ("Unknown "^prg_name^" command: "^s);
        print_avail_cmds prg_name display_map;
        exit 1
      end
    | [] -> print_need_cmd_error ()



(* externally facing *)

(* this takes an argument list, a specification list, and a usage string, does
 * the relevant parsing, and then spits out a list of anonymous arguments (those
 * not associated with command line flags. Note that one of the purposes here is
 * to mutate the prefs that are in specl, so this needs to get run first before
 * actually using any prefs.
 * *)
let wrap_parse_argv argl specl usage =
  let anonymous = ref [] in
  try
    Arg.parse_argv
      ~current:(ref 0) (* start from beginning *)
      (Array.of_list argl)
      specl
      (fun s -> anonymous := s::!anonymous)
      usage;
    List.rev !anonymous
  with
  | Arg.Bad s -> print_string s; exit 1
  | Arg.Help s -> print_string s; exit 0

(* Makes a specification with a default value.
spec_with_default "--gray-level" (fun o -> Arg.Set_int o) prefs.gray_level
"Specify the amount of gray to mix into the color scheme. Default is %d.";
 * *)
let spec_with_default symbol setfun p help =
  (symbol, setfun p, Printf.sprintf help !p)

(* given a (string, f) list, make a map of it *)
let cmd_map_of_list l =
  let longest = ref 0 in
  let display_map =
    List.map
      (fun (name, l) ->
        longest := max (String.length name) !longest;
        name,
        List.fold_right
          (fun (k, v) ->
            longest := max (String.length k) !longest;
            StringMap.add k v)
          l
          StringMap.empty)
      l
  in
  (display_map, !longest),
  List.fold_left
    (fun m1 (_, m2) -> StringMap.fold StringMap.add m1 m2)
    StringMap.empty
    display_map

(* intended to be the inner loop of a function *)
let rec inner_loop ~prg_name ~version (display_map, cmd_map) =
  let process = process_cmd prg_name display_map cmd_map
  and args = ref []
  and batchfile = ref None in
  Arg.parse
    [
      "--version", Arg.Unit (fun () -> print_endline version; exit 0),
      "Print version and exit";
      "--cmds", Arg.Unit (fun () -> print_avail_cmds prg_name display_map),
      "Print a list of the available commands.";
      "--batch", Arg.String (fun fname ->
        batchfile := Some (Batchfile.of_file fname)),
      "Run the provided batch file of guppy commands";
    ]
    (* Sys.argv and Arg.current are used here so that /this/ invocation of
       Arg.parse won't try to parse the flags that are destined for the
       subcommand. *)
    (fun _ ->
      let nargs = Array.length (Sys.argv) in
      for i = !Arg.current to (nargs - 1) do
        args := Sys.argv.(i) :: !args
      done;
      Arg.current := nargs)
    (Printf.sprintf
       "Type %s --cmds to see the list of available commands."
       prg_name);
  match !batchfile with
    | None -> process (List.rev !args)
    | Some argll ->
      let substitutions = Batchfile.split_arguments !args in
      let argll' = List.map
        (List.map (Batchfile.substitute_placeholders substitutions))
        argll
      in
      List.iter process argll'

(* the new stuff *)
exception No_default of string * string

type 'a described =
  | Needs_argument of string * string
  | Formatted of 'a * ('a -> string, unit, string) format
  | Plain of 'a * string

type 'a flag = {
  value: 'a option ref;
  opt: string;
  described: 'a described;
}

let flag opt described = {
  value = ref None;
  opt = opt;
  described = described;
}

(* fv is short for flag value. It fetches the value. *)
let fv f = match !(f.value) with
  | Some x -> x
  | None -> let x = begin match f.described with
      | Formatted (x, _) -> x
      | Plain (x, _) -> x
      | Needs_argument (name, _) -> raise (No_default (name, f.opt))
  end in f.value := Some x; x

let fvo f =
  try
    Some (fv f)
  with
    | No_default _ -> None

let desc_of_flag f =
  match f.described with
    | Needs_argument (_, s) -> s
    | Formatted (v, fmt) -> Printf.sprintf fmt v
    | Plain (_, s) -> s

let some_flag func f = f.opt, func f, desc_of_flag f
let string_flag = some_flag (fun f -> Arg.String (fun x -> f.value := Some x))
let int_flag = some_flag (fun f -> Arg.Int (fun x -> f.value := Some x))
let float_flag = some_flag (fun f -> Arg.Float (fun x -> f.value := Some x))
let toggle_flag = some_flag (fun f -> Arg.Unit (fun () -> f.value := Some (not (fv f))))
let string_list_flag = some_flag (fun f -> Arg.String (fun x -> f.value := Some (x :: fv f)))

class virtual subcommand () =
object (self)
  method virtual desc: string
  method virtual usage: string
  method virtual specl: (string * Arg.spec * string) list
  method virtual action: string list -> unit

  method run argl =
    let argl = wrap_parse_argv argl (self#specl) (self#usage) in
    try
      self#action argl
    with
      | No_default (name, opt) -> Printf.printf "no option provided for %s flag (%s)\n" name opt
end
