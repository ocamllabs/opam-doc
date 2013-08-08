OCAMLC=ocamlc
OCAMLLINK=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
OCAMLFIND=ocamlfind

OCAMLDFLAGS=-g -annot
OCAMLCFLAGS=$(OCAMLDFLAGS) -syntax "camlp4o" -package "cow,cow.syntax,compiler-libs.common,unix"

%.cmi: %.mli
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

%.cmo %.cmi: %.ml
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -c $<

all: opam-doc

clean:
	-rm *.cm[dtio]*
	-rm opam-doc
	-rm -f *~ *.annot *.js *.css *.html

opam-doc: opam_doc_config.cmo index.cmo gentyp_html.cmo doctree.cmo html_utils.cmo cmd_format.cmo generate_html.cmo driver.cmo
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^


run:opam-doc
	$(MAKE) all -C test
#	cd test; ocamldoc -html -d defaultdoc/ test.mli
#	cd test; ../opam-doc test.cmdi test.cmti
	cd test; ocamldoc -html -d default_impl/ test_impl.ml test2_impl.ml
	./opam-doc -y test/test_impl.cm[dt] test/test2_impl.cm[dt] 

run2:opam-doc
	$(MAKE) all2 -C test
#	cd test; ocamldoc -html -d defaultdoc/ test.mli
#	cd test; ../opam-doc test.cmdi test.cmti
	cd test; ocamldoc -html -d default_impl/ test_classes.ml
	./opam-doc -y test/test_classes.cm[dt]

cmd_format.cmo : doctree.cmi
cmd_format.cmx : doctree.cmx
doctree.cmo : info.cmi doctree.cmi
doctree.cmx : info.cmx doctree.cmi
doctree.cmi : info.cmi
driver.cmo : opam_doc_config.cmo index.cmi html_utils.cmo generate_html.cmi \
    cmd_format.cmo
driver.cmx : opam_doc_config.cmx index.cmx html_utils.cmx generate_html.cmx \
    cmd_format.cmx
generate_html.cmo : opam_doc_config.cmo info.cmi index.cmi html_utils.cmo \
    gentyp_html.cmi doctree.cmi docjson.cmi generate_html.cmi
generate_html.cmx : opam_doc_config.cmx info.cmx index.cmx html_utils.cmx \
    gentyp_html.cmx doctree.cmx docjson.cmx generate_html.cmi
generate_html.cmi : index.cmi doctree.cmi
gentyp_html.cmo : opam_doc_config.cmo index.cmi gentyp_html.cmi
gentyp_html.cmx : opam_doc_config.cmx index.cmx gentyp_html.cmi
gentyp_html.cmi : index.cmi
html_utils.cmo : opam_doc_config.cmo index.cmi gentyp_html.cmi
html_utils.cmx : opam_doc_config.cmx index.cmx gentyp_html.cmx
index.cmo : opam_doc_config.cmo index.cmi
index.cmx : opam_doc_config.cmx index.cmi
index.cmi :
info.cmo : info.cmi
info.cmx : info.cmi
info.cmi :
opam_doc_config.cmo :
opam_doc_config.cmx :
printast.cmo :
printast.cmx :
printdoctree.cmo : info.cmi doctree.cmi
printdoctree.cmx : info.cmx doctree.cmx
untypeast.cmo :
untypeast.cmx :
