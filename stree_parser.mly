%token CR EOF COLON SEMICOLON COMMA OPENP CLOSEP 
%token <string> LABEL REAL

%start tree
%type<Stree.info_stree> tree
%%

%{
  let node_num = ref (-1)
  let info = ref Stree.emptyInfo
  let add_tax_info taxon = 
    info := Stree.opt_add_info !node_num ~taxon !info
  let add_boot_info boot = 
    info := Stree.opt_add_info !node_num ~boot !info
  let add_bl_info bl = 
    info := Stree.opt_add_info !node_num ~bl !info
  let add_leaf leafname = 
    incr node_num;
    add_tax_info leafname;
    Stree.leaf !node_num
  let add_internal tL = 
    incr node_num;
    Stree.node !node_num tL
%}

naked_subtree:
  | LABEL
  { add_leaf $1 }
  | REAL
  { add_leaf $1 }
  | OPENP subtree_list CLOSEP
  { add_internal $2 }

subtree:
  | subtree COLON REAL
  { add_bl_info (float_of_string $3); $1 }
  | subtree REAL
  { add_boot_info (float_of_string $2); $1 }
  | naked_subtree
  { $1 }

subtree_list:
  | subtree COMMA subtree_list
  { $1 :: $3}
  | subtree
  { [$1] }
  
nosemitree: 
  | subtree 
  { 
    let result = Stree.inform_stree $1 !info in
    (* clear things out for the next tree *)
    node_num := -1;
    info := Stree.emptyInfo;
    result
  }

tree: 
  | nosemitree SEMICOLON
  { $1 }
  | nosemitree
  { $1 }
