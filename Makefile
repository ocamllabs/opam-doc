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
	-rm -f *.cm[dtio]*
	-rm -f opam-doc
	-rm -f *~ *.annot *.js *.css *.html

opam-doc: opam_doc_config.cmo index.cmo gentyp.cmo doctree.cmo html_utils.cmo cmd_format.cmo generate.cmo driver.cmo
	$(OCAMLFIND) ${OCAMLC} ${OCAMLCFLAGS} -linkpkg -o $@ $^


cmd_format.cmo : doctree.cmi
cmd_format.cmx : doctree.cmx
docjson.cmo : docjson.cmi
docjson.cmx : docjson.cmi
docjson.cmi :
doctree.cmo : info.cmi doctree.cmi
doctree.cmx : info.cmx doctree.cmi
doctree.cmi : info.cmi
driver.cmo : opam_doc_config.cmo index.cmi html_utils.cmi generate.cmi \
    cmd_format.cmo
driver.cmx : opam_doc_config.cmx index.cmx html_utils.cmx generate.cmx \
    cmd_format.cmx
generate.cmo : opam_doc_config.cmo info.cmi index.cmi html_utils.cmi \
    gentyp.cmi doctree.cmi docjson.cmi generate.cmi
generate.cmx : opam_doc_config.cmx info.cmx index.cmx html_utils.cmx \
    gentyp.cmx doctree.cmx docjson.cmx generate.cmi
generate.cmi : index.cmi doctree.cmi
gentyp.cmo : opam_doc_config.cmo index.cmi gentyp.cmi
gentyp.cmx : opam_doc_config.cmx index.cmx gentyp.cmi
gentyp.cmi : index.cmi
html_utils.cmo : opam_doc_config.cmo index.cmi gentyp.cmi html_utils.cmi
html_utils.cmx : opam_doc_config.cmx index.cmx gentyp.cmx html_utils.cmi
html_utils.cmi : opam_doc_config.cmo index.cmi gentyp.cmi
index.cmo : opam_doc_config.cmo index.cmi
index.cmx : opam_doc_config.cmx index.cmi
index.cmi :
info.cmo : info.cmi
info.cmx : info.cmi
info.cmi :
opam_doc_config.cmo :
opam_doc_config.cmx :
