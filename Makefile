RELEASE=pplacer guppy rppr
DEBUG=pplacer.d guppy.d rppr.d
DESCRIPT:=pplacer-$(shell uname)-$(shell git describe)

all: $(RELEASE)
debug: $(DEBUG)

# For OPAM
OCAML_TOPLEVEL_PATH = $$OCAML_TOPLEVEL_PATH
ifneq ($(OCAML_TOPLEVEL_PATH),)
	TOPLEVEL_FLAGS=-I ${OCAML_TOPLEVEL_PATH}
endif

$(RELEASE):
	if [ ! -e bin ]; then mkdir bin; fi
	make $@.native
	cp `readlink $@.native` bin/$@
	rm $@.native

%.native %.byte %.p.native:
	ocamlbuild $@

clean:
	rm -rf bin
	rm -f tests.native
	ocamlbuild -clean
	rm -f *.mltop

%.d: %.d.byte
	if [ ! -e bin ]; then mkdir bin; fi
	cp "$<" "bin/$@"

%.top: %.byte
	find _build -name '*.cmo' -print0 | xargs -0I% basename % .cmo > $*.mltop
	ocamlbuild $@
	rm $*.mltop

test: tests.native
	./tests.native

%.runtop: %.top
	rlwrap ./$*.top `find _build -name "*.cmi" | xargs -n1 dirname | sort -u | sed -e 's/^/-I /'` $(TOPLEVEL_FLAGS)

runcaml:
	rlwrap ocaml

tags:
	otags `find . -name "*.ml" | grep -v "_build"`

docs: gen_docs.native
	./gen_docs.native
	make -C docs html

zip: $(RELEASE)
	cp -r bin $(DESCRIPT)
	zip pplacer.zip $(DESCRIPT)/*
	rm -rf $(DESCRIPT)

.PHONY: $(RELEASE) clean runcaml tags test docs zip
