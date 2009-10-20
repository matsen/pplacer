RELEASE=mokaphy

# http://www.gnu.org/software/automake/manual/make/Quick-Reference.html
# $@ The file name of the target.
# $% The target member name, when the target is an archive member.
# $< The name of the first prerequisite.
# $? The names of all the prerequisites that are newer than the target, with spaces between them. For prerequisites which are archive members, only the member named is used (see Archives).
# $^ $+ The names of all the prerequisites, with spaces between them. For prerequisites which are archive members, only the member named is used (see Archives). The value of $^ omits duplicate prerequisites, while $+ retains them and preserves their order.
# $* The stem with which an implicit rule matches (see How Patterns Match). 

default: $(RELEASE)

$(RELEASE):
	rm -f $@.native
	make $@.native
	cp $@.native $(OCAMLDEST)/$@

%.native %.byte %.p.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean
	rm -f *.mltop

commit:
	git commit -a && git push origin master
	make -C /home/matsen/ocaml/common commit

version:
	mkvers *.ml *.mly *.mll

%.top: %.byte
	find _build -regex .*cmo | sed 's/_build\///; s/.cmo//' > $*.mltop
	ocamlbuild $@

%.runtop: %.top
	ledit -x -h .toplevel_history ./$*.top

runcaml: 
	ledit -x -h .toplevel_history ocaml

sync:
	rsync -avz --delete complete_distr.sh *.ml *.mll *.mly *.c *.clib makefile _tags --exclude common bloom:erick/pplacer_ocaml/
	rsync -avz $(FAMOCAML)/common/*.ml bloom:erick/ocaml/common/
	ssh bloom "cd erick/pplacer_ocaml && make $(RELEASE)"

stoke_release:
	make
	./complete_distr.sh
	mv *.tar.gz ../distributions

bloom_release:
	make sync
	ssh bloom "cd erick/pplacer_ocaml && ./complete_distr.sh"
	scp bloom:erick/pplacer_ocaml/*.tar.gz ../distributions

gollum_release:
	rsync -avz complete_distr.sh *.ml *.mll *.mly *.c _tags makefile --exclude myocamlbuild.ml gollum:pplacer_ocaml/
	rsync -avz $(FAMOCAML)/common/*.ml gollum:ocaml/common/
	ssh gollum "cd pplacer_ocaml && make $(MACRELEASE) && ./complete_distr.sh"
	scp gollum:pplacer_ocaml/*.tar.gz ../distributions

release:
	#make stoke_release bloom_release gollum_release
	rsync ../distributions/* armbrustlab:/var/www/html/pplacer/distributions/

tags:
	taggage *.ml ~/pplacer/ocaml/*.ml

manual:
	xpdf ~/manuals/ocaml*refman* &

# the following is just a hack so that i can say :make % in vim and have it find the place with an error
*.ml:
	ocamlc -o /dev/null $@ && rm `basename $@ .ml`.cmi && rm `basename $@ .ml`.cmo

.PHONY: $(RELEASE) clean commit version runcaml sync stoke_release bloom_release gollum_release release tags manual
