RELEASE=mokaphy placeutil placeviz pplacer

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
	rm -f tests.native
	ocamlbuild -clean
	rm -f *.mltop

%.top: %.byte
	find _build -name '*.cmo' -print0 | xargs -0I% basename % .cmo > $*.mltop
	ocamlbuild $@
	rm $*.mltop

%.runtop: %.top
	ledit -x -h .toplevel_history ./$*.top

runcaml:
	ledit -x -h .toplevel_history ocaml

test: tests.native
	./tests.native

.PHONY: $(RELEASE) clean runcaml
