(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * ftree stands for fancy tree or fantastic tree.
*)

open MapsSets

type decoration = 
  | Color of int * int * int
  | Width of float

type decor = decoration list IntMap.t

let assert_ubyte i = assert(i >= 0 || i <= 255)
let assert_ubytes = List.iter assert_ubyte
let assert_unit_interval x = assert(0. <= x && x <= 1.)

(* colors! *)
(* gray_level is the amount of gray to put in *)
let gray_red ~gray_level level = 
  assert_ubytes [gray_level; level];
  Color(level, gray_level, gray_level)

let gray_green ~gray_level level = 
  assert_ubytes [gray_level; level];
  Color(gray_level, level, gray_level)

let gray_blue ~gray_level level = 
  assert_ubytes [gray_level; level];
  Color(gray_level, gray_level, level)

let red = gray_red ~gray_level:0
let green = gray_green ~gray_level:0
let blue = gray_blue ~gray_level:0

let rev_color = function
  | Color(r,g,b) -> 
      assert_ubytes [r;g;b];
      let rev x = 255 - x in
      Color(rev r, rev g, rev g)
  | Width _ as width -> width

(* "color" is actually any map from an int to a decoration *)
let scaled_color color ~min ~max x = 
  assert_unit_interval x;
  color (min + (int_of_float ((float_of_int (max - min)) *. x)))


(* width *)

let width w = Width w

let scaled_width ~min ~max x = 
  assert_unit_interval x;
  width (min +. (max -. min) *. x)


(* decors *)

let get_decoration_list d id =
  Base.get_from_list_intmap id d

let empty_decor = IntMap.empty
