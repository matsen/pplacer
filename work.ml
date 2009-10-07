open MapsSets

let make_zero_leaf bl taxon = 
  Stree.inform_stree 
    (Stree.leaf 0)
    (Stree.opt_add_info 0 ~bl ~taxon Stree.emptyInfo)

let t1 = make_zero_leaf 10. "one"
let t2 = make_zero_leaf 2. "two"
let t3 = make_zero_leaf 3. "three"
let t4 = make_zero_leaf 4. "four"

let m =
  IntMapFuns.of_pairlist_listly 
  [
    0, (2.,t2); 
    0, (5.,t3)
  ]

let x = Stree.add_boosted_subtree_above ~t:t1 ~new_t:t2 0.25 5
let id,x = Stree.add_subtrees_above 1 t1 [2., t2; 5., t3]
let i = Stree.get_info x
let x = Stree.add_subtrees_by_map t1 m
let id,start = Stree.add_subtrees_above 1 t1 [4., t2;]
let i = Stree.get_info start
let t = Stree.get_tree start

let m =
  IntMapFuns.of_pairlist_listly 
  [
    0, (1.,t4); 
    0, (1.,t3);
    2, (1.,t3);
  ]

let id,x = Stree.add_subtrees_above 3 start [0.5, t3; 2., t4]
let i = Stree.get_info x
let t = Stree.get_tree x

let finish = Stree.add_subtrees_by_map start m
let i = Stree.get_info finish
