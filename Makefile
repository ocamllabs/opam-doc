OCAMLC=ocamlc
OCAMLLINK=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLFIND=ocamlfind

OCAMLCFLAGS=-g -annot -syntax "camlp4o" -package "cow,cow.syntax,compiler-libs.common,unix"

%.cmi: %.mli
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

all: opam-doc

clean:
	-rm *.cmi *.cmo
	-rm opam-doc

opam-doc: opam_doc_config.cmo index.cmo gentyp.cmo doctree.cmo docjson.cmo html_utils.cmo doc_html.cmo doc_html_bis.cmo cmd_format.cmo generate.cmo driver.cmo
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^

run:opam-doc
	$(MAKE) all -C test
#	cd test; ocamldoc -html -d defaultdoc/ test.mli
#	cd test; ../opam-doc test.cmdi test.cmti
	cd test; ocamldoc -html -d default_impl/ test_include.ml test_impl.ml test2_impl.ml
	cd test; ../opam-doc test_include.cm[dt] test_impl.cm[dt] test2_impl.cm[dt] 


opam_doc_config.cmo:
docjson.cmi :
doctree.cmi : info.cmi
html_utils.cmi: docjson.cmi docjson.cmi opam_doc_config.cmi
gentyp.cmi : index.cmi
index.cmi : docjson.cmi
info.cmi :
cmd_format.cmo : doctree.cmi
doc_html_bis.cmo : docjson.cmi html_utils.cmi
doc_html.cmo : docjson.cmi
docjson.cmo : docjson.cmi
doctree.cmo : info.cmi doctree.cmi
driver.cmo : index.cmi generate.cmo doctree.cmi docjson.cmi doc_html_bis.cmo \
 doc_html.cmo cmd_format.cmo  opam_doc_config.cmi
generate.cmo : info.cmi gentyp.cmi doctree.cmi docjson.cmi
gentyp.cmo : index.cmi gentyp.cmi
index.cmo : index.cmi
info.cmo : info.cmi
printast.cmo :
printdoctree.cmo : info.cmi doctree.cmi
untypeast.cmo :
