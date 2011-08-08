%token CR EOF COLON SEMICOLON COMMA OPENP CLOSEP
%token <int> EDGE_LABEL
%token <float> REAL
%token <string> LABEL

%start tree
%type<Newick_bark.newick_bark Gtree.gtree> tree
%%

%{
  open Ppatteries

  (* parse state *)
  type 'a ps = {
    stree: Stree.stree;
    bark: 'a IntMap.t;
  }
  (* list parse state *)
  type 'a lps = {
    stree_l: Stree.stree list;
    bark_l: 'a IntMap.t;
  }

  let combine = IntMap.fold IntMap.add

  let node_num = ref (-1)
  let add_bark add_fun ?label x s =
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
      bark = IntMap.empty;
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
        IntMap.fold (fun k v m ->
          IntMap.add (if k = old_id then $2 else k) v m
        ) $1.bark IntMap.empty
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
