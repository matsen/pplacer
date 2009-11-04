(* Copyright (C) 2009  Frederick A Matsen.
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * a multiset module. mem tests for membership, while n_mem returns the number
 * of times something is bound.
 *)

open MapsSets

module Multiset (OT: Map.OrderedType) =
  struct
    module M = Map.Make(OT)

    type t = int M.t
    let empty = M.empty
    let mem = M.mem
    let find = M.find

    let n_mem x s = 
      if mem x s then find x s
      else 0

    let add x s = 
      if mem x s then M.add x ((M.find x s)+1) s
      else M.add x 1 s

    let remove x s = 
      if mem x s then begin
        let new_val = (M.find x s) - 1 in
        assert(new_val >= 0);
        if new_val = 0 then
          M.remove x s
        else
          M.add x new_val s
      end
      else raise Not_found

    let of_list = List.fold_left (fun s x -> add x s) empty

      (* sends the multiset to a list _with multiplicity_ *)
    let to_list s = 
      let rec multiadd x n l = 
        if n <= 0 then l
        else multiadd x (n-1) (x::l)
      in
      List.rev (M.fold multiadd s [])


  end


module FloatMultiset = Multiset(OrderedFloat)
module IntMultiset = Multiset(OrderedInt)
module CharMultiset = Multiset(OrderedChar)
module StringMultiset = Multiset(OrderedString)


module MultisetFuns (OT: Map.OrderedType) (SBLE: STRINGABLE with type t = OT.t) =
  struct
    module MF = MapFuns(OT)(SBLE)

    let ppr = MF.ppr_int
  end


module FloatMultisetFuns = MultisetFuns(OrderedFloat)(StringableFloat)
module IntMultisetFuns = MultisetFuns(OrderedInt)(StringableInt)
module CharMultisetFuns = MultisetFuns(OrderedChar)(StringableChar)
module StringMultisetFuns = MultisetFuns(OrderedString)(StringableString)
