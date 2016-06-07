.SUFFIXES: .c .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean library_code

OUT=main

$(OUT):

MOC=`qmake -query QT_INSTALL_BINS`/moc
LABLQML=../../lablqml
CC=g++ -g -fPIC -std=c++11 `pkg-config --cflags Qt5Quick` -I$(LABLQML)/_build -I`ocamlc -where` \
   -Dprotected=public -Dprivate=public
CLINK=g++ -g `pkg-config --cflags Qt5Quick Qt5Widgets` -fPIC
CLINKLIBS=`pkg-config --libs Qt5Quick Qt5Widgets`
QTCLASSES=controller item
GEN_H=
GEN_CMX=
GEN_MOC=
GEN_CPP=
GEN_CPP_OBJS=
#MOC_CPP= #$(addprefix moc_,$(GEN_CMX:.cmx=_c.cpp) )
PPX_QT=../ppx_qt
OCAMLOPT=ocamlfind opt -package compiler-libs.common,unix -linkpkg -g

CMX=program.cmx
CMX_TEST=

define PPXRULES
GEN_CMX+=$(1).cmx
GEN_H+=$(1).h
GEN_CPP+=$(1)_c.cpp
$(1).h: $(1).ml
	ocamlopt -I $(LABLQML)/_build/ -c -dsource -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

GEN_CPP+=moc_$(1).cpp
moc_$(1).cpp: $(1).h
	$(MOC) $(1).h -o moc_$(1).cpp

GEN_CPP_OBJS+=$(1)_c.o
$(1)_c.o: $(1)_c.cpp
	$(CC) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

GEN_CPP_OBJS+=moc_$(1).o
moc_$(1)_c.o: moc_$(1)_c.cpp
	$(CC) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

endef
$(foreach i,$(QTCLASSES),$(eval $(call PPXRULES,$(i)) ) )

$(OUT): $(GEN_H) $(GEN_CPP_OBJS) $(GEN_CMX) $(CMX) camlcode.o  main.o
	$(CLINK) -L`ocamlc -where` \
	$(LABLQML)/_build/stubs.o $(LABLQML)/_build/startup_stubs.o $(GEN_CPP_OBJS) camlcode.o main.o -lasmrun \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(OCAMLOPT) -output-obj -dstartup -I $(LABLQML)/_build $(LABLQML)/_build/lablqml.cmxa str.cmxa \
	$(GEN_CMX) $(CMX) -linkall -o camlcode.o



moc_%.cpp: %.h
	$(MOC) $< > $@

.cpp.o:
	$(CC) -c $< -I.

.ml.cmx:
	$(OCAMLOPT) -I $(LABLQML)/_build -c $<

clean:
	rm -f *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) $(GEN_H) $(GEN_CPP) $(OUT)

-include  $(shell ocamlc -where)/Makefile.config
#include .depend
