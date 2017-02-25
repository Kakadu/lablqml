OB=ocamlbuild -use-ocamlfind

ifeq ($(VERBOSE),1)
OB+= -classic-display
endif

.PHONY: all clean

all:
	env OCAMLPATH=$$OCAMLPATH:`pwd`/../../lib/_build/bundle \
		$(OB) src/qrc_resources.c src/propMapTest.native

clean:
	rm -fr _build *.native
