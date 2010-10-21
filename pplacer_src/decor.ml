(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * decor gives types and routines for attaching information to trees
*)

open MapsSets

(* note that order is important so the xml comes out right when we sort using
 * polymorphic compare *)
type decoration = 
  | Width of float
  | Color of int * int * int
  | Dot of int
  | Taxinfo of Tax_id.tax_id * string

let assert_ubyte i = assert(i >= 0 || i <= 255)
let assert_ubytes = List.iter assert_ubyte
let assert_unit_interval x = assert(0. <= x && x <= 1.)

(* colors! 255 is the most saturated. *)
let color (r, g, b) = assert_ubytes [r; g; b]; Color(r, g, b)

let white = color (255,255,255)
let black = color (0,0,0)
let red = color (255,0,0)
let orange = color (255, 165, 0)
let yellow = color (255, 255, 0)
let green = color (0, 255, 0)
let blue = color (0, 0, 255)

(* white is 255, black is 0 *)
let gray intensity = color (intensity, intensity, intensity)

(* weight is the weight of a *)
let int_avg weight a b = 
  assert(0. <= weight && weight <= 1.);
  int_of_float 
    ((float_of_int a) *. weight +. (float_of_int b) *. (1. -. weight))
let triple_weighted_avg weight (r1,g1,b1) (r2,g2,b2) =
  (int_avg weight r1 r2, int_avg weight g1 g2, int_avg weight b1 b2)

(* weight is the weight of a *)
let color_avg weight c1 c2 = 
  match (c1,c2) with
  | (Color (r1,g1,b1), Color (r2,g2,b2)) ->
      color (triple_weighted_avg weight (r1,g1,b1) (r2,g2,b2))
  | _ -> assert(false)

(*
(* gray_level is the amount of gray to put in *)
let gray_red ~gray_level level = color level gray_level gray_level
let gray_green ~gray_level level = color gray_level level gray_level
let gray_blue ~gray_level level = color gray_level gray_level level

(* "a_color" is actually any map from an int to a decoration *)
let scaled_color a_color ~min ~max x = 
  assert_unit_interval x;
  a_color (min + (int_of_float ((float_of_int (max - min)) *. x)))
*)


(* width *)

let width w = Width w

let scaled_width ~min ~max x = 
  assert_unit_interval x;
  width (min +. (max -. min) *. x)


(* dot *)

let dot i = Dot i


(* Taxinfo *)

let taxinfo ti name = Taxinfo (ti, name)


(* writing *)

let ppr ff = function
  | Color(r,g,b) -> Format.fprintf ff "Color(%d, %d, %d)" r g b
  | Width w -> Format.fprintf ff "Width(%g)" w
  | Dot i -> Format.fprintf ff "Dot(%d)" i
  | Taxinfo (ti,name) -> Format.fprintf ff "Taxinfo(%a,%s)" Tax_id.ppr ti name

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
  | Taxinfo (ti, name) -> 
     Xml.write_long_tag
       (fun () ->
         Tax_id.write_xml ch ti;
         Xml.write_string "scientific_name" ch name)
       "taxonomy"
       ch

