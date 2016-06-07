.SUFFIXES: .c .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean library_code

PPX_QT=ppx_qt.native
OCAMLC=ocamlc
OCAMLOPT:=ocamlfind opt -package lablqml
CXXFLAGS=-std=c++11 `pkg-config --cflags Qt5Quick` -fPIC
OUT=main
QT_MODULES=Qt5Quick Qt5Widgets
#CLINKLIBS = -cclib -lstdc++
#CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-L     $(QT_MODULES) ) )
#CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-other $(QT_MODULES) ) )
#CLINKLIBS+= $(addprefix -cclib , $(shell pkg-config --libs-only-l     $(QT_MODULES) ) )
CLINKLIBS = `pkg-config --libs Qt5Quick Qt5Widgets`
CLINK=g++ -g `pkg-config --cflags Qt5Quick Qt5Widgets` -fPIC

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c
ifeq ($(VERBOSE),1)
V=
else
V=@
endif

GENERATED_H=

all: generate $(OUT)

define MAKE_RULES
GENERATED_H += $(1).h
GENERATED_CPP += $(1)_c.cpp
GENERATED_MOC += moc_$(1).cpp
GENERATED_SOURCES += $(1)_c.cpp moc_$(1).cpp
GENERATED_CMX += $(1).cmx
$(1).h: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1).cmx: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -ppx $(PPX_QT) -c $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(V)$(CXX) $(CXXFLAGS) -Dprotected=public -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	$(V)moc $(1).h -o moc_$(1).cpp

endef
QTCLASSES=controller intModel dataItem
$(foreach i,$(QTCLASSES),$(eval $(call MAKE_RULES,$(i)) ) )

GEN_CPP_OBJS=$(GENERATED_SOURCES:.cpp=.o)
CMX=program.cmx
generate: $(GENERATED_H)


$(OUT): $(GENERATED_H) $(GEN_CPP_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o  main.o
	$(CLINK) -L`ocamlc -where` \
	$(GEN_CPP_OBJS)  \
	-L../../lablqml/_build/bundle/lablqml \
	camlcode.o main.o -lasmrun -llablqml_stubs \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	lablqml.cmxa $(GENERATED_CMX) $(CMX)  -linkall -o camlcode.o

main.o: CXXFLAGS += -I../../lablqml
#.cpp.o:
#	$(CXX) $(CXXFLAGS) -I`ocamlc -where` -I../../lablqml -c $< -I.

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean:
	rm -f *.o *.cm[oiax] *.cmxa *.o.startup.s $(GENERATED_SOURCES) $(GENERATED_H) $(OUT)

-include  $(shell ocamlc -where)/Makefile.config
#include .depend
