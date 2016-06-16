OB=ocamlbuild -use-ocamlfind  -j 2 #-classic-display

.PHONY: all clean

all:
	$(OB) src/qrc_resources.c src/propMapTest.native

clean:
	rm -fr _build *.native
