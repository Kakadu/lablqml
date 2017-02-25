include ../Makefile.rule


.PHONY: all clean library_code generate app

OUT=prop_test.native
CLINK=g++ -g `pkg-config --cflags Qt5Quick Qt5Widgets` -fPIC

GENERATED_H=

.DEFAULT_GOAL: all
all: $(OUT)

QTCLASSES=controller intModel dataItem
$(call MAKE_RULES,$(QTCLASSES))

CMX=program.cmx

LABLQML_WHERE=$(shell PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	ocamlfind query lablqml)

$(OUT): $(GENERATED_H) $(GENERATED_OBJS) $(GENERATED_CMX) $(CMX) camlcode.o main.o
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(CLINK) -L`ocamlfind c -where` \
	$(GENERATED_OBJS) -L$(LABLQML_WHERE) \
	camlcode.o main.o -lasmrun -llablqml_stubs \
	-lunix -lcamlstr $(LDFLAGS) $(LDLIBS) $(NATIVECCLIBS)  -o $(OUT)

camlcode.o: $(GEN_CMX) $(CMX)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup \
	lablqml.cmxa $(GENERATED_CMX) $(CMX)  -linkall -o camlcode.o

main.o: CXXFLAGS += -I$(LABLQML_WHERE) #for stubs.h
main.o: main.cpp
	$(V)$(CXX) $(CXXFLAGS) -I`ocamlfind c -where` -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -c $<

clean::
	$(RM) *.o *.cm[oiax] *.cmxa *.o.startup.s $(OUT)
