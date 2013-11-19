OCAMLBUILD?=ocamlbuild
J?=4

.PHONY: all clean

all: opam-doc bin-doc opam

opam-doc: src/opam-doc/*.mli src/opam-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/opam-doc/driver.native
	mv driver.native opam-doc

bin-doc: src/bin-doc/*.mli src/bin-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/bin-doc/driver.native
	mv driver.native bin-doc

opam:
	mkdir opam
	opam init -n --root=opam

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean
	rm -fr opam