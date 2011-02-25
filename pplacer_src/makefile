default:
	make -C .. all

%:
	make -C .. $@

tags:
	taggage `find /home/matsen/pplacer/ocaml -name "*.ml" | grep -v "_build"`


.PHONY: tags
