include ../Makefile.rule

OCAMLFIND_PACKAGES+= str

OUT=main
GENERATED_FILES=

.PHONY: clean all generate app
.DEFAULT_GOAL: all
all: $(OUT)

LABLQML=../../lablqml
LABLQML_WHERE= `OCAMLPATH=../../lablqml/_build/bundle ocamlfind query lablqml`
CLINK=g++ -g `pkg-config --cflags Qt5Quick Qt5Widgets` -fPIC
CLINKLIBS=`pkg-config --libs Qt5Quick Qt5Widgets`
CMX=program.cmx
CMX_TEST=

QTCLASSES=controller item
$(call MAKE_RULES,$(QTCLASSES))

$(OUT): $(GENERATED_H) $(GENERATED_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o  main.o
	$(V)$(CLINK) -L`ocamlc -where` \
	$(GENERATED_OBJS) camlcode.o main.o -lasmrun \
	-L$(LABLQML_WHERE) -llablqml_stubs \
	-lunix -lcamlstr $(CLINKLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	$(GENERATED_CMX) $(CMX) -linkpkg -linkall -o camlcode.o

main.o: CXXFLAGS += -Dprotected=public
main.o: CXXFLAGS += -I$(LABLQML_WHERE) #for lablqml.h
main.o: main.cpp
	$(V)$(CXX) $(CXXFLAGS) -I`ocamlfind c -where` -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT)  -c $<

clean::
	$(RM) *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) $(GEN_H) $(GEN_CPP) $(OUT)
