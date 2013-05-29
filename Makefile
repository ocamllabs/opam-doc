all:
	obuild build

run: *.ml
	cd test; $(MAKE)
	dist/build/opamdoc/opamdoc test/test.cmdi test/test.cmti test/test2.cmdi test/test2.cmti