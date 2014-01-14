MOC=`qmake -query QT_INSTALL_BINS`/moc
LABLQML=../lablqml
#QT_MODULES=Qt5Quick Qt5Qml Qt5Widgets
CC=g++ -g -fPIC -std=c++0x `pkg-config --cflags Qt5Quick Qt5Qml Qt5Widgets` \
   -I$(LABLQML)/_build -I`ocamlc -where` \
   -Dprotected=public -Dprivate=public

CLINK=g++ -g
CLINKLIBS=`pkg-config --libs Qt5Quick Qt5Qml Qt5Widgets`
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

all: $(GEN_CMX) $(CMX) $(CMX_TEST) library_code $(GEN_MOC) $(GEN_CPP) moc_TestQObject.cpp TestQObject.o moc_TestQObject.o main.o MyDynamicQObject.o
	$(CLINK) -L`ocamlc -where` \
	$(LABLQML)/_build/stubs.o $(LABLQML)/_build/startup_stubs.o TestQObject.o moc_TestQObject.o MyDynamicQObject.o $(GEN_CPP) camlcode.o main.o -lasmrun \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

depend:
	ocamlfind dep *.ml *.ml > .depend

library_code:
	$(OCAMLOPT) -output-obj -dstartup -I $(LABLQML)/_build $(LABLQML)/_build/lablqml.cmxa str.cmxa \
	$(GEN_CMX) $(CMX) -linkall -o camlcode.o

moc_%.cpp: %.h
	echo $(MOC)
	$(MOC) $< > $@

.cpp.o: %.h
	echo $(MOC)
	$(CC) -c $< -I.

.ml.cmx:
	$(OCAMLOPT) -I $(LABLQML)/_build -c $<

clean:
	rm *.o *.cm[oitax] *.cmxa *.o.startup.s $(MOC_CPP) -f

-include  $(shell ocamlc -where)/Makefile.config
include .depend
