# In this makefile we make a library from OCaml code.
# It is not required in this concrete case but can be useful if we do
# start-up from C
PPX_QT=ppx_qt.native

OCAMLFIND_PACKAGES=lablqml,compiler-libs.common,str
OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package $(OCAMLFIND_PACKAGES) -thread
CXXFLAGS=-std=c++0x `pkg-config --cflags Qt5Quick` -fPIC

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

GENERATED_H=

all: generate $(OUT)

define MAKE_RULES
GENERATED_H += $(1).h
GENERATED_SOURCES += $(1)_c.cpp
GENERATED_CPP += $(1)_c.o
GENERATED_MOC += moc_$(1).o
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

GEN_TARGETS=controller abstractModel dataItem
$(foreach i,$(GEN_TARGETS),$(eval $(call MAKE_RULES,$(i)) ) )
GEN_CMX=$(addsuffix .cmx,$(GEN_TARGETS) )
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

$(OUT): generate $(GEN_CMX) $(CMX) $(GEN_MOC) $(GEN_OBJS)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg \
	$(GEN_OBJS)  $(CLINKLIBS) \
	$(GEN_CMX) $(CMX) \
	-o $(OUT)

library_code:
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -output-obj -dstartup -I $(LABLQML)/_build $(LABLQML)/_build/lablqml.cmxa str.cmxa \
	$(V)$(GEN_CMX) $(CMX) -linkall -o camlcode.o

.ml.cmx:
	$(V)OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean:
	$(RM) *.o *.cm[oiax] *.cmxa *.o.startup.s $(MOC_CPP) -f

-include  $(shell ocamlc -where)/Makefile.config
