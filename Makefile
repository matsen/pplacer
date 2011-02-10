RELEASE=placeviz pplacer placeutil mokaphy

all: $(RELEASE)

$(RELEASE):
	if [ ! -e bin ]; then mkdir bin; fi
	make $@.native
	cp `readlink $@.native` bin/$@
	rm $@.native

%.native %.byte %.p.native:
	ocamlbuild $@

clean:
	rm -rf bin libs
	ocamlbuild -clean
	rm *.mltop

%.top: %.byte
	find _build -name '*.cmo' -print0 | xargs -0I% basename % .cmo > $*.mltop
	ocamlbuild $@

%.runtop: %.top
	ledit -x -h .toplevel_history ./$*.top

runcaml:
	ledit -x -h .toplevel_history ocaml

.PHONY: $(RELEASE) clean runcaml
