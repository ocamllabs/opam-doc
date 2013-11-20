OCAMLBUILD?=ocamlbuild
J?=4

.PHONY: all clean

all: doc

opam-doc: src/opam-doc/*.mli src/opam-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/opam-doc/driver.native
	mv driver.native opam-doc

bin-doc: src/bin-doc/*.mli src/bin-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/bin-doc/driver.native
	mv driver.native bin-doc

opam: scripts/ocamlc scripts/ocamlc.opt
	mkdir opam
	opam init -n --root=opam
	cp scripts/ocamlc opam/system/bin/
	cp scripts/ocamlc.opt opam/system/bin/

data: opam bin-doc scripts/collect-data.sh
	scripts/collect-data.sh

doc: data opam-doc scripts/create-docs.sh
	scripts/create-docs.sh

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean
	rm -fr opam
	rm -fr data
	rm -fr doc