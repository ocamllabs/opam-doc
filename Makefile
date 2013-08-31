BINDIR=./bin

OCAMLC=ocamlc
OCAMLLINK=ocamlc
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLFIND=ocamlfind

COMMON_FLAGS=-syntax "camlp4o" -package "cow,cow.syntax,compiler-libs.common,unix"
OCAMLDFLAGS=-g -annot
OCAMLCFLAGS=$(OCAMLDFLAGS) $(COMMON_FLAGS)
OCAMLDEPFLAGS=$(COMMON_FLAGS)

%.cmi: %.mli
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

all: opam-doc

clean:
	-rm -f *.cm[dtio]*
	-rm -f opam-doc
	-rm -f *~ *.annot *.js *.css *.html

install: all
	-mkdir -p ${BINDIR}
	cp opam-doc ${BINDIR}/opam-doc

opam-doc: opam_doc_config.cmo index.cmo gentyp.cmo doctree.cmo html_utils.cmo cmd_format.cmo generate.cmo driver.cmo
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^

depend: 
	$(OCAMLFIND) $(OCAMLDEP) $(OCAMLDEPFLAGS) *.mli *.ml > .depend

-include .depend
