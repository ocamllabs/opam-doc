OCAMLC=ocamlc
OCAMLLINK=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLFIND=ocamlfind

OCAMLCFLAGS=-g -annot -syntax "camlp4o" -package "cow,cow.syntax,compiler-libs.common"

%.cmi: %.mli
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

all: opam-doc

clean:
	-rm *.cmi *.cmo
	-rm opam-doc

opam-doc: index.cmo gentyp.cmo  doctree.cmo docjson.cmo doc_html.cmo cmd_format.cmo generate.cmo driver.cmo 
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^

run:opam-doc
	$(MAKE) all -C test
#	cd test; ocamldoc -html -d defaultdoc/ test.mli
#	cd test; ../opam-doc test.cmdi test.cmti
	cd test; ocamldoc -html -d default_impl/ test_impl.ml
	cd test; ../opam-doc test_impl.cmd test_impl.cmt test2_impl.cmd test2_impl.cmt; 

docjson.cmi :
doctree.cmi : info.cmi
gentyp.cmi : index.cmi
index.cmi :
info.cmi :
cmd_format.cmo : doctree.cmi
doc_html.cmo : docjson.cmi
docjson.cmo : docjson.cmi
doctree.cmo : info.cmi doctree.cmi
driver.cmo : index.cmi generate.cmo doctree.cmi docjson.cmi doc_html.cmo \
    cmd_format.cmo
generate.cmo : info.cmi gentyp.cmi doctree.cmi docjson.cmi
gentyp.cmo : index.cmi gentyp.cmi
index.cmo : index.cmi
info.cmo : info.cmi
printast.cmo :
printdoctree.cmo : info.cmi doctree.cmi
untypeast.cmo :
