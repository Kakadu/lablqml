OB=ocamlbuild -use-ocamlfind -classic-display
TARGETS=lablqml.cmxa lablqml.a QmlContext.cmi liblablqml_stubs.a dlllablqml_stubs.so
INSTALL_TARGETS=$(addprefix _build/,$(TARGETS))

all:
	$(OB) liblablqml_stubs.a lablqml.cmxa

clean:
	$(RM) -r _build

install:
	ocamlfind install lablqml META $(INSTALL_TARGETS)

uninstall:
	ocamlfind remove lablqml
