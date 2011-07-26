open Ptree
open Pd

let pl_of_hash h = Hashtbl.fold (fun k v l -> (k,v)::l) h []

let unit_bl_of_s s =
  let st = Gtree.get_stree (Newick.of_string s) in
  of_stree (fun _ -> 1.) st

let test_to_from s =
  let st = Gtree.get_stree (Newick.of_string s) in
  let pt = of_stree (fun _ -> 1.) st in
  (s, pl_of_hash pt, st, to_gtree pt);;

let test s =
  let pt = unit_bl_of_s s in
  let ps = idblset_of_ptree pt in
  (pl_of_hash pt, IdblSet.elements ps);;

let x = test "((x,y),(a,b))";;
let x = test_to_from "((x,y),(a,b))";;

let x = test "((x,y),((a,b),(c,d)))";;
let x = test_to_from "((x,y),((a,b),(c,d)))";;

(*
let four = of_string "((x:1,y:2):4,(a:9,b:9):9,(c:9,d:9):9):9";;
let x = PendSet.elements (pendset_of_pt four);;
let x = pl_of_hash four;;
let x = perform four 7.;;

let six = of_string "((x:1,y:2):4,(a:9,b:9):9,(c:9,d:9):9):9";;
let x = to_gtree six;;
let s = idblset_of_pt six;;
let s = delete_pend six 0 s;; let x = (IdblSet.elements s, pl_of_hash six, to_gtree six);;
let s = delete_pend six 3 s;; let x = (IdblSet.elements s, pl_of_hash six, to_gtree six);;
*)

let six = of_string "((x:1,y:2):1,(a:1,b:9):9,(c:9,d:9):9):9;";;
let x = until_stopping six 7.;;

(* let pt = of_file "COG0001.auto1.fast.tre" *)
(* let out = perform pt 1e-2 *)
