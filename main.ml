(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)


let ppr_info_stree_list ff stl = 
  Ppr.ppr_list Stree.ppr_info_stree ff stl


let () = 
  let lexbuf = Lexing.from_channel (open_in "test.txt") in
  let a = Stree_parser.main Stree_lexer.token lexbuf in
  assert(a <> []);
  ppr_info_stree_list Format.std_formatter a


