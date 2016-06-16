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

include ../Makefile.rule

QTCLASSES=controller item
$(foreach i,$(QTCLASSES),$(eval $(call MAKE_RULES,$(i)) ) )

$(OUT): $(GENERATED_H) $(GENERATED_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o  main.o
	$(V)$(CLINK) -L`ocamlc -where` \
	$(GENERATED_OBJS) camlcode.o main.o -lasmrun \
	-L../../lablqml/_build/bundle/lablqml -llablqml_stubs \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	$(GENERATED_CMX) $(CMX) -linkpkg -linkall -o camlcode.o

main.o: CXXFLAGS += -Dprotected=public
main.o: CXXFLAGS += -I../../lablqml #for stubs.h
main.o: main.cpp
	$(V)$(CXX) $(CXXFLAGS) -I`ocamlfind c -where` -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT)  -c $<

clean:
	$(RM) *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) $(GEN_H) $(GEN_CPP) $(OUT)

-include  $(shell ocamlc -where)/Makefile.config
