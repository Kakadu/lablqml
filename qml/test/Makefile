MOC=`qmake -query QT_INSTALL_BINS`/moc
CC=g++ -g -fPIC -std=c++0x `pkg-config --cflags Qt5Core` -I../lib/_build -I`ocamlc -where` \
   -Dprotected=public -Dprivate=public
CLINK=g++ -g
CLINKLIBS_=-lstdc++ -L$(shell qmake -query QT_INSTALL_LIBS) -lQt5Quick -lQt5Gui -lQt5Qml -lQt5Network -lQt5Core
CLINKLIBS=$(addprefix -ccopt , $(CLINKLIBS_))
OUT=main
GEN_CMX=DataItem.cmx AbstractModel.cmx Controller.cmx
MOC_CPP=$(addprefix moc_,$(GEN_CMX:.cmx=_c.cpp) )
GEN_CPP=$(GEN_CMX:.cmx=_c.o) $(MOC_CPP:.cpp=.o)
GEN_MOC=$(GEN_CMX:.cmx=_c.cpp)
OCAMLOPT=ocamlfind opt -package compiler-libs.common,unix -linkpkg -g

CMX=helpers.cmx tree.cmx S.cmx Richify.cmx program.cmx
CMX_TEST=Test.cmx

.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

all: $(GEN_CMX) $(CMX) $(GEN_MOC) $(GEN_CPP)
	$(OCAMLOPT) -package str -linkpkg \
	 $(GEN_CPP)  $(CLINKLIBS) \
	-ccopt -L../lib/_build/ ../lib/_build/lablqml.cmxa $(GEN_CMX) $(CMX) -o $(OUT)

depend:
	ocamlfind dep *.ml *.ml > .depend

library_code:
	$(OCAMLOPT) -output-obj -dstartup -I ../lib/_build ../lib/_build/lablqml.cmxa str.cmxa \
	$(GEN_CMX) $(CMX) -linkall -o camlcode.o #-verbose

moc_%.cpp: %.h
	$(MOC) $< > $@

.cpp.o:
	$(CC) -c $< -I.

.ml.cmx:
	$(OCAMLOPT) -I ../lib/_build -c $<

clean:
	rm *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) -f

-include  $(shell ocamlc -where)/Makefile.config
include .depend
