PPX_QT=ppx_qt.native

OCAMLFIND_PACKAGES=lablqml,compiler-libs.common,str
OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package $(OCAMLFIND_PACKAGES)
CXXFLAGS=-std=c++11 `pkg-config --cflags Qt5Quick` -fPIC

OUT=main
QT_MODULES=Qt5Quick Qt5Widgets
CLINKLIBS = -cclib -lstdc++
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-L     $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-other $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -cclib , $(shell pkg-config --libs-only-l     $(QT_MODULES) ) )
GENERATED_FILES=

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c
ifeq ($(VERBOSE),1)
V=
else
V=@
endif

all: $(OUT)

LABLQML=../../lablqml
CLINK=g++ -g `pkg-config --cflags Qt5Quick Qt5Widgets` -fPIC
CLINKLIBS=`pkg-config --libs Qt5Quick Qt5Widgets`
CMX=program.cmx
CMX_TEST=

define MAKE_RULES
GENERATED_H += $(1).h
GENERATED_SOURCES += $(1)_c.cpp
GENERATED_CPP += $(1)_c.o
GENERATED_MOC += moc_$(1).o
GENERATED_OBJS += $(1)_c.o moc_$(1).o
GENERATED_CMX += $(1).cmx
$(1).h: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1).cmx: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c -ppx "$(PPX_QT) -nocpp" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(V)$(CXX) $(CXXFLAGS) -Dprotected=public -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	$(V)moc $(1).h -o moc_$(1).cpp

endef

QTCLASSES=controller item
$(foreach i,$(QTCLASSES),$(eval $(call MAKE_RULES,$(i)) ) )

$(OUT): $(GENERATED_H) $(GENERATED_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o  main.o
	$(CLINK) -L`ocamlc -where` \
	$(GENERATED_OBJS) camlcode.o main.o -lasmrun \
	-L../../lablqml/_build/bundle/lablqml -llablqml_stubs \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	$(GENERATED_CMX) $(CMX) -linkpkg -linkall -o camlcode.o

main.o: CXXFLAGS += -Dprotected=public -I../../lablqml

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT)  -c $<

clean:
	rm -f *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) $(GEN_H) $(GEN_CPP) $(OUT)

-include  $(shell ocamlc -where)/Makefile.config
#include .depend
