(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Bitfield
open AlgMap

(* bitfields treated like numbers *)
module BfNum = 
struct 
  type t = Bitfield.t
  let of_int = of_int
  let to_int = to_int
  let of_float x = of_int (int_of_float x)
  let to_float x = float_of_int (to_int x)
  let neg x = x
  let add = ( lxor )
  let sub = add
  let mul = ( land )
  let div _ _ = empty (* the fun stops here *)
  let max = ( lor )
  let pow _ _ = empty (* could put something here but ... *)
  let indic x y = if x = y then 1 else 0
  let compare = compare
  let succ x = to_int (1 + of_int x)
  let abs x = x
  let inv _ = empty
  let exp _ = empty
  let log _ = empty
  let zero = empty
  let one = of_int 1
  let to_string = to_string
  let of_string _ = failwith "not implemented"
end

module BfAlgXMap = AlgMap (BfNum)

(* so this is a AlgMap from ints to bfs *)
module BfAlgIntMap = BfAlgXMap (MapsSets.OrderedInt)

module OrderedBitfield = struct
  type t = Bitfield.t
  let compare = Bitfield.compare
end

module StringableBitfield = struct
  type t = Bitfield.t
  let to_string = Bitfield.to_string
end

module BFAMR = AlgMapR (OrderedBitfield)
module BitfieldMapFuns = MapFuns (OrderedBitfield) (StringableBitfield)
