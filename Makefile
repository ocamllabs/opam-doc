OCAMLBUILD?=ocamlbuild
J?=4

.PHONY: all serve clean

all: doc

opam-doc: src/opam-doc/*.mli src/opam-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/opam-doc/driver.native
	mv driver.native opam-doc

bin-doc: src/bin-doc/*.mli src/bin-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/bin-doc/driver.native
	mv driver.native bin-doc

opam: scripts/ocamlc scripts/ocamlc.opt bin-doc
	rm -fr opam
	mkdir opam
	opam init -n --root=opam
	cp scripts/ocamlc opam/system/bin/
	cp scripts/ocamlc.opt opam/system/bin/
	cp bin-doc opam/system/bin/
	scripts/add-repos.sh

data: opam scripts/collect-data.sh packages
	scripts/collect-data.sh

doc: data opam-doc scripts/create-docs.sh url
	scripts/create-docs.sh "doc" `cat url | grep -v '^#'`

serve-doc: data opam-doc scripts/create-docs.sh
	scripts/create-docs.sh "serve-doc" "http://127.0.0.1:8000"

serve: serve-doc
	-cd serve-doc; python -m SimpleHTTPServer

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean
	-rm -fr opam
	-rm -fr data
	-rm -fr doc
	-rm -fr serve-doc