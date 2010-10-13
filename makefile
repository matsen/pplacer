default:
	make -f Makefile 
	cp bin/* $(OCAMLDEST)

tags:
	taggage /home/matsen/pplacer/ocaml/*/*.ml

%:
	make -f Makefile $@


.PHONY: tags
