(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * for making and aligning matrices of strings
 *)
 
open Fam_batteries

let range n = Array.init n (fun i -> i)

let pad_to_width width s = 
  StringFuns.left_pad (width - String.length s) ' ' s

let column_widths m =
  MatrixFuns.assert_rectangular m;
  let widths = MatrixFuns.map String.length m 
  and n_rows = MatrixFuns.n_rows m
  and n_cols = MatrixFuns.n_cols m
  in
  let all_rows = range n_rows in
  Array.map
    (fun col ->
      Array.fold_right
        (fun row -> max widths.(row).(col))
        all_rows
        0)
    (range n_cols)

let pad m = 
  let widths = column_widths m in
  MatrixFuns.mapij 
    (fun _ j s -> pad_to_width widths.(j) s)
    m

(*
# add_names [|"a";"b"|] [|[|"0";"1"|];[|"2";"3"|]|];;
- : string array array =
  [|[|""; "a"; "b"|]; [|"a"; "0"; "1"|]; [|"b"; "2"; "3"|]|]
*)
let add_names name_arr m = 
  MatrixFuns.assert_rectangular m;
  let n_rows = MatrixFuns.n_rows m 
  and n_cols = MatrixFuns.n_cols m in
  assert(n_rows = n_cols && n_cols = Array.length name_arr);
  MatrixFuns.init
    (n_rows+1)
    (n_cols+1)
    (fun i j -> 
      if (i,j) = (0,0) then ""
      else if min i j = 0 then name_arr.((max i j) - 1)
      else m.(i-1).(j-1))

let m = add_names [|"a";"b"|] [|[|"0";"1"|];[|"2";"3"|]|]

let row_to_str delim row = 
  String.concat delim (Array.to_list row)

let write_row ch row = 
  Printf.fprintf ch "%s\n" (row_to_str "  " row)

let write_padded ch m = 
  Array.iter (write_row ch) (pad m)

let write_named_padded ch names m = 
  Array.iter (write_row ch) (pad (add_names names m))


