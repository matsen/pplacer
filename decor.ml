(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
*)

open MapsSets

type decoration = 
  | Color of int * int * int
  | Width of float
  | Dot of int

let assert_ubyte i = assert(i >= 0 || i <= 255)
let assert_ubytes = List.iter assert_ubyte
let assert_unit_interval x = assert(0. <= x && x <= 1.)

(* colors! 255 is the most saturated. *)
let color r g b = assert_ubytes [r; g; b]; Color(r, g, b)

(* gray_level is the amount of gray to put in *)
let gray_red ~gray_level level = color level gray_level gray_level
let gray_green ~gray_level level = color gray_level level gray_level
let gray_blue ~gray_level level = color gray_level gray_level level

let red_f = gray_red ~gray_level:0
let green_f = gray_green ~gray_level:0
let blue_f = gray_blue ~gray_level:0

let red = red_f 255
let orange = color 255 165 0
let yellow = color 255 255 0
let green = green_f 255
let blue = blue_f 255

let rev_color = function
  | Color(r,g,b) -> 
      assert_ubytes [r;g;b];
      let rev x = 255 - x in
      Color(rev r, rev g, rev b)
  | Width _ as width -> width
  | Dot _ as dot -> dot

(* "a_color" is actually any map from an int to a decoration *)
let scaled_color a_color ~min ~max x = 
  assert_unit_interval x;
  a_color (min + (int_of_float ((float_of_int (max - min)) *. x)))


(* width *)

let width w = Width w

let scaled_width ~min ~max x = 
  assert_unit_interval x;
  width (min +. (max -. min) *. x)


(* dot *)

let dot i = Dot i


(* writing *)

let ppr ff = function
  | Color(r,g,b) -> Format.fprintf ff "Color(%d, %d, %d)" r g b
  | Width w -> Format.fprintf ff "Width(%g)" w
  | Dot i -> Format.fprintf ff "Dot(%d)" i

let write_xml ch = function
  | Color(r,g,b) -> 
      Xml.write_long_tag
        (fun () -> 
          Xml.write_int "red" ch r;
          Xml.write_int "green" ch g;
          Xml.write_int "blue" ch b;)
        "color"
        ch
  | Width w -> 
      Xml.write_float "width" ch w
  | Dot i -> 
      let tag_name = 
        let r = i mod 3 in
        if r = 0 then "duplications"
        else if r = 1 then "speciations"
        else "losses"
      in
      Xml.write_long_tag
        (fun () -> Xml.write_int tag_name ch (i+1);)
        "events"
        ch
