MOC=$$QT5/bin/moc
CC=g++ -fPIC -std=c++0x -I$$QT5_ROOT/qtbase/include -I../_build -I`ocamlc -where` \
   -Dprotected=public -Dprivate=public
CLINK=g++ -L$$QT5_ROOT/qtbase/lib -lQt5Core -lQt5Qml -lQt5Quick -lQt5Gui
OUT=main
GEN_CMX=DataItem.cmx AbstractModel.cmx Controller.cmx
MOC_CPP=$(addprefix moc_,$(GEN_CMX:.cmx=_c.cpp) )
GEN_CPP=$(GEN_CMX:.cmx=_c.o) $(MOC_CPP:.cpp=.o)
GEN_MOC=$(GEN_CMX:.cmx=_c.cpp)
OCAMLOPT=ocamlfind opt -package compiler-libs.common,unix -linkpkg

CMX=helpers.cmx tree.cmx S.cmx program.cmx
CMX_TEST=Test.cmx

.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

all: $(GEN_CMX) $(CMX) $(CMX_TEST) library_code $(GEN_MOC) $(GEN_CPP) main.o
	$(OCAMLOPT) helpers.cmx tree.cmx S.cmx Test.cmx -o test
	$(CLINK) -L`ocamlc -where` \
	../_build/stubs.o $(GEN_CPP) camlcode.o main.o $(NATIVECCLIBS) -lasmrun \
	-lunix -o $(OUT)

depend: gen_stubs
	ocamlfind dep *.ml *.ml > .depend

library_code:
	$(OCAMLOPT) -output-obj -dstartup -I ../_build ../_build/qml_wrap.cmxa \
	$(GEN_CMX) $(CMX) -linkall -o camlcode.o #-verbose

gen_stubs: input_yaml
	./mocml -qml_wrap

moc_%.cpp: %.h
	$(MOC) $< > $@

.cpp.o:
	$(CC) -c $< -I.

.ml.cmx:
	$(OCAMLOPT) -I ../_build -c $<

clean:
	rm *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) -f

-include  $(shell ocamlc -where)/Makefile.config
include .depend
