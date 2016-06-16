include  $(shell ocamlc -where)/Makefile.config

#export PATH=$(shell echo ${PATH}):../../src
#export OCAML_PATH=/home/kakadu/lablqt/lablqml/_build/bundle
# WTF. We can't export variable to subshells in make
PPX_QT=ppx_qt

OCAMLC=ocamlc
OCAMLOPT:=ocamlfind opt -package lablqml
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC -I`ocamlc -where`
OUT=helloworld
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

include ../Makefile.rule
$(foreach i,controller,$(eval $(call MAKE_RULES,$(i)) ) )

generate: $(GENERATED_H)

$(OUT): $(GENERATED_CMX) program.cmx $(GENERATED_CPP) $(GENERATED_MOC)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $^ -o $(OUT)

clean_gen:
	$(V)$(RM) $(GENERATED_H) $(GENERATED_SOURCES)

clean: clean_gen
	$(V)$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)

#.cpp.o:
#	$(V)$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<
