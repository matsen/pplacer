(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocamlbuild_plugin;; 
open Command;; 


dispatch begin function
  | Before_options ->
      (* use static linking for native binaries *)
      flag ["link"; "ocaml"; "native";] (S[A"-ccopt"; A"-static"]);

  | After_rules ->
      (* choose your compiler *)
      flag ["compile"; "c"; ] (S[A"-cc"; A"/usr/bin/gcc";]);

      (* custom: incorporate libraries into bytecode *)
      flag ["link"; "ocaml"; "byte"; ] (A"-custom");

      (* link with libpplacercside given use_pplacer tag *)
      flag ["link"; "ocaml"; "use_pplacer"; ]
        (S[A"-cclib"; A"-lpplacercside"; A"-cclib"; A"-L.";]);

      (* make libpplacercside when needed *)
      dep ["use_pplacer"; ] ["libpplacercside.a"; ];

      (* automatically include gsl when the use_gsl tag is given in _tags *)
      ocaml_lib ~extern:true ~dir:"+gsl" "gsl";

      (* link with atlas given use_gsl tag 
      flag ["link"; "ocaml"; "use_gsl"]
        (S[A"-cclib"; A"-lcblas"; A"-cclib"; A"-latlas"; A"-cclib"; A"-L/usr/lib/sse2/";]);
        (S[A"-cclib"; A"-lcblas"; A"-cclib"; A"-latlas"; A"-cclib"; A"-L/home/rkodner/erick/atlas/build/lib";]);
       *)
  | _ -> ()
end;;


