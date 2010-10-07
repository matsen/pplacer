# That this makefile is just for making the interactive toplevel with the common files built in.

default: runtop

common.mltop:
	ls *.ml | sed 's/\.ml//' > common.mltop

common.top: *.ml common.mltop
	ocamlbuild common.top

runtop: common.top
	ledit -x -h .toplevel_history ./common.top

runcaml: 
	ledit -x -h .toplevel_history ocaml
