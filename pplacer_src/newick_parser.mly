%token CR EOF COLON SEMICOLON COMMA OPENP CLOSEP
%token <int> EDGE_LABEL
%token <float> REAL
%token <string> LABEL

%start tree
%type<Newick_bark.newick_bark Gtree.gtree> tree
%%

%{
(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
*)

  (* parse state *)
  type 'a ps = {
    stree: Stree.stree;
    bark: 'a MapsSets.IntMap.t;
  }
  (* list parse state *)
  type 'a lps = {
    stree_l: Stree.stree list;
    bark_l: 'a MapsSets.IntMap.t;
  }

  let combine = MapsSets.IntMap.fold MapsSets.IntMap.add

  let node_num = ref (-1)
  let add_bark add_fun ?(label = None) x s =
    let label =
      match label with
        | None -> Stree.top_id s.stree
        | Some x -> x
    in {
      stree = s.stree;
      bark = add_fun label x s.bark;
    }
  let add_bl = add_bark Newick_bark.map_set_bl
  let add_name = add_bark Newick_bark.map_set_name
  let add_boot = add_bark Newick_bark.map_set_boot
  let add_leaf leafname =
    incr node_num;
    add_name leafname {
      stree = Stree.leaf !node_num;
      bark = MapsSets.IntMap.empty;
    }
  let add_internal ls =
    incr node_num;
    {
      stree = Stree.node !node_num ls.stree_l;
      bark = ls.bark_l;
    }
%}

naked_subtree:
  | LABEL
  { add_leaf $1 }
  | REAL
  { add_leaf (string_of_float $1) }
  | OPENP subtree_list CLOSEP
  { add_internal $2 }

subtree:
  | subtree EDGE_LABEL
  {
    {
      stree = Stree.of_id $2 $1.stree;
      bark =
        let old_id = Stree.top_id $1.stree in
        MapsSets.IntMap.fold (fun k v m ->
          MapsSets.IntMap.add (if k = old_id then $2 else k) v m
        ) $1.bark MapsSets.IntMap.empty
    }
  }
  | subtree COLON REAL
  { add_bl $3 $1 }
  | subtree REAL
  { add_boot $2 $1 }
  | naked_subtree
  { $1 }

subtree_list:
  | subtree COMMA subtree_list
  {
    {
      stree_l = $1.stree :: $3.stree_l;
      bark_l = combine $1.bark $3.bark_l;
    }
  }
  | subtree
  {
    {
      stree_l = [$1.stree];
      bark_l = $1.bark;
    }
  }

nosemitree:
  | subtree
  {
    node_num := (-1);
    Gtree.gtree $1.stree $1.bark
  }

tree:
  | nosemitree SEMICOLON
  { $1 }
  | nosemitree
  { $1 }
