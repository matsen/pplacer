(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocamlbuild_plugin;; 
open Command;; 


(* the following two functions are a hack to properly include the library
 * depenencies until GODI upgrades to ocamlbuild 3.12, which works nicely with
 * ocamlfind. *)

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
    while true do Buffer.add_channel buf ic 1 done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  Filename.chop_suffix (Buffer.contents buf) "\n"
in

let ocamlfind_query pkg = 
  syscall (Printf.sprintf "ocamlfind query %s" (Filename.quote pkg))
in


dispatch begin function
  | Before_options ->
      (* use static linking for native binaries *)
      flag ["link"; "ocaml"; "native";] (S[A"-ccopt"; A"-static"]);

  | After_rules ->
      (* c compilation options *)
      flag ["compile"; "c"; ] 
      (S[
        A"-cc"; A"/usr/bin/gcc";
        A"-ccopt"; A"-Wall";
        A"-ccopt"; A"-funroll-loops";
        A"-ccopt"; A"-O3";
        A"-ccopt"; A"-fPIC";
      ]);

      (* custom: incorporate libraries into bytecode *)
      flag ["link"; "ocaml"; "byte"; ] (A"-custom");

      (* link with libpplacercside given use_pplacer tag *)
      flag ["link"; "ocaml"; "use_pplacer"; ]
        (S[A"-cclib"; A"-lpplacercside"; A"-cclib"; A"-Lpplacer_src";]);

      (* make libpplacercside when needed *)
      dep ["use_pplacer"; ] ["pplacer_src/libpplacercside.a"; ];

      (* automatically include <lib> when the use_<lib> tag is given in _tags *)
      ocaml_lib ~extern:true ~dir:(ocamlfind_query "json") "json";
      ocaml_lib ~extern:true ~dir:(ocamlfind_query "gsl") "gsl";
      ocaml_lib ~extern:true ~dir:(ocamlfind_query "csv") "csv";
  | _ -> ()
end;;

