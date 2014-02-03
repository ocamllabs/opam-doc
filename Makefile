OCAMLBUILD     ?= ocamlbuild
OCAMLC         = $(shell which ocamlc)
COMPILER      ?= system
BIN            = $(shell opam config var bin)
OPAMROOT       = $(shell opam config var root)

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
	sed -e "s|__OCAMLC__|${OCAMLC}|" < scripts/ocamlc > $(BIN)/opam-doc-ocamlc
	sed -e "s|__OCAMLC__|${OCAMLC}|" < scripts/ocamlc.opt > $(BIN)/opam-doc-ocamlc.opt
	chmod +x $(BIN)/opam-doc-ocamlc
	chmod +x $(BIN)/opam-doc-ocamlc.opt
	cp bin-doc $(BIN)
	cp scripts/opam-doc-collect.sh $(BIN)/opam-doc-collect
	cp scripts/opam-doc-create.sh $(BIN)/opam-doc-create
	cp opam-doc-index $(BIN)/opam-doc-index
	cp scripts/opam-doc-serve.sh $(BIN)/opam-doc-serve
	cp scripts/opam-doc.sh $(BIN)/opam-doc
	rm -rf $(OPAMROOT)/doc-static
	mkdir -p $(OPAMROOT)/doc-static
	cp -r static/* $(OPAMROOT)/doc-static/

uninstall:
	rm -f $(BIN)/opam-doc-collect \
	  $(BIN)/opam-doc-create \
	  $(BIN)/opam-doc-serve \
	  $(BIN)/opam-doc

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean
