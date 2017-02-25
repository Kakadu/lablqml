OUT=qocamlbrowser
OB=ocamlbuild -use-ocamlfind

ifeq ($(VERBOSE),1)
OB+= -classic-display
endif

.PHONY: all clean install uninstall

all:
	PATH=`pwd`/../../src/:$$PATH OCAMLPATH=`pwd`/../../lib/_build/bundle \
	$(OB) src/moc_controller.c src/qrc_resources.c src/libcppstubs.a src/program.native


clean:
	rm -fr _build program.native

install:
	cp _build/src/program.native $(PREFIX)/bin/$(OUT)

uninstall:
	rm -fr $(PREFIX)/bin/$(OUT)
