# In this makefile we make a library from OCaml code.
# It is not required in this concrete case but can be useful if we do
# start-up from C
PPX_QT=ppx_qt

OCAMLFIND_PACKAGES=lablqml,compiler-libs.common,str
OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package $(OCAMLFIND_PACKAGES) -thread
CXXFLAGS=-std=c++11 `pkg-config --cflags Qt5Quick` -fPIC -I`ocamlc -where`

OUT=test
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

all: generate $(OUT)

include ../Makefile.rule

GEN_CPP=$(GEN_CMX:.cmx=_c.c)
GEN_MOC=$(addprefix moc_,$(GEN_CMX:.cmx=.cpp) )
GEN_OBJS=$(GEN_CPP:.c=.o) $(GEN_MOC:.cpp=.o)

generate: $(GENERATED_H)

MOC=`qmake -query QT_INSTALL_BINS`/moc
OUT=test
CMX=helpers.cmx tree.cmx S.cmx Richify.cmx program.cmx
CMX_TEST=Test.cmx

.SUFFIXES: .c .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

$(OUT): generate $(GENRATED_CMX) $(CMX) $(GENERATED_MOC) $(GENERATED_CPP)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg \
	$(GENERATED_CPP) $(GENERATED_MOC) $(CLINKLIBS) \
	$(GENERATED_CMX) $(CMX) \
	-o $(OUT)

library_code:
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(V)$(OCAMLOPT) -output-obj -dstartup $(GEN_CMX) $(CMX) -linkall -o camlcode.o

.ml.cmx:
	$(V)OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean:
	$(RM) -f *.o *.cm[oiax] *.cmxa *.o.startup.s \
	$(GENERATED_SOURCES) $(GENERATED_H) $(MOC_CPP)

-include  $(shell ocamlc -where)/Makefile.config
