
oldoldall:
#	ocamlc -I ~/.opam/4.00.1/lib/lablgtk2 lablgtk.cma test.mli test2.mli
#	ocamldoc -d defaultdoc -html test.mli  test2.mli
	ocamlc -I ~/.opam/4.00.1/lib/lablgtk2 lablgtk.cma -bin-annot test.mli test2.mli
	bin-doc test.mli test2.mli

all:
	ocamlc -bin-annot test_include.ml test_impl.ml test2_impl.ml 
	../../bin-doc/bin-doc test_include.ml test_impl.ml test2_impl.ml

clean:
	-rm -f *~ a.out
	-rm -f *.cm[io]
	-rm -f test.cmti test2.cmti test.cmdi test2.cmdi
	-rm -f test_impl.cmt test2_impl.cmt test_impl.cmd test2_impl.cmd
	-rm -f *.json
	-rm -f *.html style.css doc_loader.js
	-rm -f *.html.contents
	-rm -f global.index

