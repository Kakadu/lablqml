CP=cp -v
OB=ocamlbuild -use-ocamlfind -classic-display

.PHONY: all bundle clean ppx_qt_exe ppx_qt_archive

all: ppx_qt_exe bundle

ppx_qt_archive:
	$(OB) ppxext/ppx_qt.cmxa ppxext/ppx_qt.cma

ppx_qt_exe: ppx_qt_archive
	$(OB) ppxext/ppx_qt_main.native

BUNDLEDIR := ../lib/_build/bundle/lablqml
bundle:
	@$(CP) _build/ppxext/ppx_qt_main.native $(BUNDLEDIR)/ppx_qt_main
	@$(CP) _build/ppxext/ppx_qt.cmx         $(BUNDLEDIR)
	@$(CP) _build/ppxext/ppx_qt.cmxa        $(BUNDLEDIR)
	@$(CP) _build/ppxext/ppx_qt.a           $(BUNDLEDIR)

clean:
	$(RM) -r _build *.native ./ppx_qt
