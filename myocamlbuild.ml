(* pplacer v0.3. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocamlbuild_plugin;; 
open Command;; 


dispatch begin function
  | After_rules ->
      (* custom: incorporate libraries into bytecode *)
      flag ["link"; "ocaml"; "byte"] (A"-custom");
      (* automatically include gsl when the use_gsl tag is given in _tags *)
      ocaml_lib ~extern:true ~dir:"+gsl" "gsl";
  | _ -> ()
end;;


(*
dep tags deps:
  Will build deps when all tags will be activated.

flag tags command_spec:
  Will inject the given piece of command (command_spec) when all tags are activated.

ocaml_lib <options> library_pathname:
  Declare an ocaml library.
Example: ocaml_lib "foo/bar" This will setup the tag use_bar tag. At link time it will include: foo/bar.cma or foo/bar.cmxa If you supply the ~dir:"boo" option -I boo will be added at link and compile time. Use ~extern:true for non-ocamlbuild handled libraries. Use ~byte:false or ~native:false to disable byte or native mode. Use ~tag_name:"usebar" to override the default tag name.

*)
