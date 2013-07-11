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

opam-doc: opam_doc_config.cmo index.cmo gentyp.cmo doctree.cmo docjson.cmo html_utils.cmo doc_html.cmo cmd_format.cmo generate.cmo driver.cmo
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^

run:opam-doc
	$(MAKE) all -C test
#	cd test; ocamldoc -html -d defaultdoc/ test.mli
#	cd test; ../opam-doc test.cmdi test.cmti
	cd test; ocamldoc -html -d default_impl/ test_include.ml test_impl.ml test2_impl.ml
	cd test; ../opam-doc test_include.cm[dt] test_impl.cm[dt] test2_impl.cm[dt] 

cmd_format.cmo : doctree.cmi
cmd_format.cmx : doctree.cmx
doc_html.cmo : opam_doc_config.cmo index.cmi html_utils.cmo docjson.cmi
doc_html.cmx : opam_doc_config.cmx index.cmx html_utils.cmx docjson.cmx
docjson.cmo : docjson.cmi
docjson.cmx : docjson.cmi
docjson.cmi :
doctree.cmo : info.cmi doctree.cmi
doctree.cmx : info.cmx doctree.cmi
doctree.cmi : info.cmi
driver.cmo : opam_doc_config.cmo index.cmi generate.cmo docjson.cmi \
    doc_html.cmo cmd_format.cmo
driver.cmx : opam_doc_config.cmx index.cmx generate.cmx docjson.cmx \
    doc_html.cmx cmd_format.cmx
generate.cmo : info.cmi index.cmi gentyp.cmi doctree.cmi docjson.cmi
generate.cmx : info.cmx index.cmx gentyp.cmx doctree.cmx docjson.cmx
gentyp.cmo : opam_doc_config.cmo index.cmi gentyp.cmi
gentyp.cmx : opam_doc_config.cmx index.cmx gentyp.cmi
gentyp.cmi : index.cmi
html_utils.cmo : opam_doc_config.cmo docjson.cmi
html_utils.cmx : opam_doc_config.cmx docjson.cmx
index.cmo : index.cmi
index.cmx : index.cmi
index.cmi : docjson.cmi
info.cmo : info.cmi
info.cmx : info.cmi
info.cmi :
old_doc_html.cmo : docjson.cmi
old_doc_html.cmx : docjson.cmx
opam_doc_config.cmo :
opam_doc_config.cmx :
printast.cmo :
printast.cmx :
printdoctree.cmo : info.cmi doctree.cmi
printdoctree.cmx : info.cmx doctree.cmx
untypeast.cmo :
untypeast.cmx :
