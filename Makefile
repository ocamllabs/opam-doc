BINDIR=./bin
TARGET=driver
BINNAME=opamdoc
OCAMLBUILD?=ocamlbuild
J?=4

.PHONY: all clean install

all: _build/$(TARGET).byte _build/$(TARGET).native

_build/$(TARGET).byte _build/$(TARGET).native:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind $(TARGET).byte $(TARGET).native

clean:
	$(OCAMLBUILD) -j $(J) -use-ocamlfind -clean

install:
	mkdir -p $(BINDIR)
	cp $(TARGET).byte $(BINDIR)/$(BINNAME)
	cp $(TARGET).native $(BINDIR)/$(BINNAME).opt
