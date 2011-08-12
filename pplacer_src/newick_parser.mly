%token EOF COLON SEMICOLON COMMA OPENP CLOSEP
%token <int> EDGE_LABEL
%token <float> REAL
%token <string> LABEL

%start tree
%type <Newick_bark.newick_bark Gtree.gtree> tree
%%

%{
open Ppatteries

(* parse state *)
type ps = {
  stree: Stree.stree;
  bark: Newick_bark.newick_bark IntMap.t;
  my_bark: Newick_bark.newick_bark IntMap.t;
}
(* list parse state *)
type lps = {
  stree_l: Stree.stree list;
  bark_l: Newick_bark.newick_bark IntMap.t;
  my_bark_l: Newick_bark.newick_bark IntMap.t;
}

let combine = IntMap.fold IntMap.add
let empty_lps = {stree_l = []; bark_l = IntMap.empty; my_bark_l = IntMap.empty}
let lps_append lp lps =
  {lps with
    stree_l = lp.stree :: lps.stree_l;
    bark_l = combine lp.bark lp.my_bark |> combine lps.bark_l}

let node_num = ref (-1)
let add_bark add_fun x s =
  {s with my_bark = add_fun (Stree.top_id s.stree) x s.my_bark}
let add_bl = add_bark Newick_bark.map_set_bl
let add_name = add_bark Newick_bark.map_set_name
let add_boot = add_bark Newick_bark.map_set_boot
let add_id id lp =
  {lp with
    stree = Stree.of_id id lp.stree;
    my_bark =
      try
        let value, bark' = Stree.top_id lp.stree
          |> flip IntMap.extract lp.my_bark
        in
        IntMap.add id value bark'
      with Not_found -> lp.my_bark
  }

let add_leaf () =
  incr node_num;
  add_bl 0. {
    stree = Stree.leaf !node_num;
    bark = IntMap.empty;
    my_bark = IntMap.empty;
  }
let add_internal ls =
  incr node_num;
  add_bl 0. {
    stree = Stree.node !node_num ls.stree_l;
    bark = combine ls.bark_l ls.my_bark_l;
    my_bark = IntMap.empty;
  }

let reset () =
  node_num := (-1)

%}

named_leaf:
  | LABEL
      { add_leaf () |> add_name $1 }
  | REAL
      { add_leaf () |> add_name (string_of_float $1) }

lengthy_leaf:
  | COLON REAL
      { add_leaf () |> add_bl $2 }
  | named_leaf COLON REAL
      { add_bl $3 $1 }
  | named_leaf { $1 }

leaf:
  | EDGE_LABEL
      { add_leaf () |> add_id $1 }
  | lengthy_leaf EDGE_LABEL
      { add_id $2 $1 }
  | lengthy_leaf { $1 }

subtree_list:
  | subtree COMMA subtree_list
      { lps_append $1 $3 }
  | subtree
      { lps_append $1 empty_lps }

bare_subtree_group:
  | OPENP subtree_list CLOSEP
      { add_internal $2 }

named_subtree_group:
  | bare_subtree_group LABEL
      { add_name $2 $1 }
  | bare_subtree_group REAL
      { add_name (string_of_float $2) $1 }
  | bare_subtree_group { $1 }

lengthy_subtree_group:
  | named_subtree_group COLON REAL
      { add_bl $3 $1 }
  | named_subtree_group { $1 }

subtree_group:
  | lengthy_subtree_group EDGE_LABEL
      { add_id $2 $1 }
  | lengthy_subtree_group { $1 }

subtree: /* empty */ { add_leaf () }
  | subtree_group { $1 }
  | leaf { $1 }

bare_tree:
  | subtree SEMICOLON
      { $1 }
  | subtree { $1 }

tree:
  | bare_tree EOF
      { reset (); Gtree.gtree $1.stree (combine $1.bark $1.my_bark) }
  | error EOF
      { reset (); Sparse.syntax_error 1 "syntax error parsing" }
