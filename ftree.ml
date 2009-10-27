(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * ftree stands for fancy tree or fantastic tree.
*)

open MapsSets

type ftree = 
  {
    itree : Itree.itree;
    decor : Decor.decor;
  }

let make itree decor = { itree = itree; decor = decor; }

let get_itree t = t.itree
let get_decor t = t.decor

let get_decoration_list t id = 
  Decor.get_decoration_list (get_decor t) id


