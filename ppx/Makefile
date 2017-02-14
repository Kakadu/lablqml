OB=ocamlbuild -use-ocamlfind -classic-display

.PHONY: all clean ppx_qt.native

all: ppx_qt

ppx_qt: ppx_qt.native
	ln -sf ./ppx_qt.native ./ppx_qt

ppx_qt.native:
	$(OB) ppxext/ppx_qt.native

clean:
	$(RM) -r _build *.native ./ppx_qt
