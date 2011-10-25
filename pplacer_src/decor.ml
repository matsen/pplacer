(* decor gives types and routines for attaching information to trees
*)

open Ppatteries

(* note that order is important so the xml comes out right when we sort using
 * polymorphic compare *)
type decoration =
  | Width of float
  | Color of int * int * int
  | Dot of int
  | Taxinfo of Tax_id.tax_id * string

type decor = decoration list

let assert_ubyte i = assert(i >= 0 || i <= 255)
let assert_ubytes = List.iter assert_ubyte
let assert_unit_interval x = assert(0. <= x && x <= 1.)

(* colors! 255 is the most saturated. *)
let color (r, g, b) = assert_ubytes [r; g; b]; Color(r, g, b)

let random_color () =
  let cv () = Random.int 192 + 64 in
  color (cv (), cv (), cv ())

(* basic colors *)
let white = color (255,255,255)
let black = color (0,0,0)
let red = color (255,0,0)
let orange = color (255, 165, 0)
let yellow = color (255, 255, 0)
let green = color (0, 255, 0)
let blue = color (0, 0, 255)

(* interesting colors *)
let sand = color (255, 90, 90)

(* Set2 from colorbrewer.org *)
let brew_orange = color (252, 141, 98)
let brew_green = color (102, 194, 165)
let brew_blue = color (141, 160, 203)

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
  match c1, c2 with
    | Color (r1,g1,b1), Color (r2,g2,b2) ->
      color (triple_weighted_avg weight (r1,g1,b1) (r2,g2,b2))
    | _ -> invalid_arg "color_avg"

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

let to_xml = function
  | Color (r, g, b) ->
    [Xml.Element ("color", [], [
      Myxml.tag "red" (string_of_int r);
      Myxml.tag "green" (string_of_int g);
      Myxml.tag "blue" (string_of_int b);
    ])]
  | Width w ->
    [Myxml.tag "width" (Printf.sprintf "%g" w)]
  | Dot i ->
    let tag_name = match i mod 3 with
      | 0 -> "duplications"
      | 1 -> "speciations"
      | _ -> "losses"
    in [Xml.Element ("events", [], [
      Myxml.tag tag_name (string_of_int (i + 1));
    ])]
  | Taxinfo (ti, name) ->
    [Xml.Element ("taxonomy", [], Tax_id.to_xml ti @ [
      Myxml.tag "scientific_name" name;
    ])]

