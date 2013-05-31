MOC=`qmake -query QT_INSTALL_BINS`/moc
CC=g++ -g -fPIC -std=c++0x `pkg-config --cflags Qt5Core` -I${LABLQT}/qml/lib/_build -I`ocamlc -where` \
   -Dprotected=public -Dprivate=public
CLINK=g++ -g
CLINKLIBS=`pkg-config --libs Qt5Quick`
OUT=main
GEN_CMX=
MOC_CPP=$(addprefix moc_,$(GEN_CMX:.cmx=_c.cpp) )
GEN_CPP=$(GEN_CMX:.cmx=_c.o) $(MOC_CPP:.cpp=.o)
GEN_MOC=$(GEN_CMX:.cmx=_c.cpp)
OCAMLOPT=ocamlfind opt -package compiler-libs.common,unix -linkpkg -g

CMX=program.cmx
CMX_TEST=

.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

all: $(GEN_CMX) $(CMX) $(CMX_TEST) library_code $(GEN_MOC) $(GEN_CPP) main.o
	$(CLINK) -L`ocamlc -where` \
	${LABLQT}/qml/lib/_build/stubs.o $(GEN_CPP) camlcode.o main.o -lasmrun \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

depend:
	ocamlfind dep *.ml *.ml > .depend

library_code:
	$(OCAMLOPT) -output-obj -dstartup \
	-I ${LABLQT}/qml/lib/_build ${LABLQT}/qml/lib/_build/qml_wrap.cmxa str.cmxa \
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