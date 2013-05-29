all:
	ocamldoc -d defaultdoc -html test.mli test2.mli
	ocamlc -bin-annot test.mli test2.mli
	bin-doc test.mli test.ml test2.mli test2.ml 

clean:
	-rm *~
	-rm *.cm*
	-rm a.out
	-rm *.json

