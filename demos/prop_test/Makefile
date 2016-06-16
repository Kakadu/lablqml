.SUFFIXES: .c .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean library_code

PPX_QT=ppx_qt
OCAMLC=ocamlc
OCAMLOPT:=ocamlfind opt -package lablqml
CXXFLAGS=-std=c++11 `pkg-config --cflags Qt5Quick` -fPIC -I`ocamlc -where`
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

all: $(OUT)

include ../Makefile.rule
QTCLASSES=controller intModel dataItem
$(foreach i,$(QTCLASSES),$(eval $(call MAKE_RULES,$(i)) ) )

CMX=program.cmx

$(OUT): $(GENERATED_H) $(GENERATED_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o main.o
	$(V)$(CLINK) -L`ocamlfind c -where` \
	$(GENERATED_OBJS) -L`ocamlfind query lablqml` \
	camlcode.o main.o -lasmrun -llablqml_stubs \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	lablqml.cmxa $(GENERATED_CMX) $(CMX)  -linkall -o camlcode.o

main.o: CXXFLAGS += -I../../lablqml #for stubs.h
main.o: main.cpp
	$(V)$(CXX) $(CXXFLAGS) -I`ocamlfind c -where` -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean:
	rm -f *.o *.cm[oiax] *.cmxa *.o.startup.s $(GENERATED_SOURCES) $(GENERATED_H) $(OUT)

-include  $(shell ocamlc -where)/Makefile.config
