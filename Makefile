OCAMLBUILD     ?= ocamlbuild
OCAMLC         = $(shell which ocamlc)
COMPILER      ?= system
DOCBIN         = $(shell opam config var root)/doc/bin
BIN            = $(shell opam config var bin)

J?=4

.PHONY: all serve clean

all: opam-doc-index bin-doc
	@

opam-doc-index: src/opam-doc-index/*.mli src/opam-doc-index/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/opam-doc-index/driver.native
	mv driver.native opam-doc-index

bin-doc: src/bin-doc/*.mli src/bin-doc/*.ml
	$(OCAMLBUILD) -j $(J) -use-ocamlfind src/bin-doc/driver.native
	mv driver.native bin-doc

install: scripts/ocamlc scripts/ocamlc.opt bin-doc opam-doc-index
	opam switch doc -A $(COMPILER) --no-switch
	sed -e "s+__OCAMLC__+${OCAMLC}+" < scripts/ocamlc > $(DOCBIN)/ocamlc
	sed -e "s+__OCAMLC__+${OCAMLC}+" < scripts/ocamlc.opt > $(DOCBIN)/ocamlc.opt
	chmod +x $(DOCBIN)/ocamlc
	chmod +x $(DOCBIN)/ocamlc.opt
	cp bin-doc $(DOCBIN)
	cp scripts/opam-doc-collect.sh $(BIN)/opam-doc-collect
	cp scripts/opam-doc-create.sh $(BIN)/opam-doc-create
	cp opam-doc-index $(BIN)/opam-doc-index
	cp scripts/opam-doc-serve.sh $(BIN)/opam-doc-serve
	cp scripts/opam-doc.sh $(BIN)/opam-doc

uninstall:
	opam switch remove doc
	rm -f $(BIN)/opam-doc-collect \
	  $(BIN)/opam-doc-create \
	  $(BIN)/opam-doc-serve \
	  $(BIN)/opam-doc

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean
