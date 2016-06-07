#export PATH=$(shell echo ${PATH}):../../src
#export OCAML_PATH=/home/kakadu/lablqt/lablqml/_build/bundle
# WTF. We can't export variable to subshells in make
PPX_QT=ppx_qt.native

OCAMLC=ocamlc
OCAMLOPT:=ocamlfind opt -package lablqml
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC
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

$(1)_c.o: $(1)_c.cpp
	$(V)$(CXX) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	$(V)moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,controller,$(eval $(call MAKE_RULES,$(i)) ) )

generate: $(GENERATED_H)

$(OUT): $(GENERATED_CMX) program.cmx $(GENERATED_CPP) $(GENERATED_MOC)
	PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $^ -o $(OUT)

clean_gen:
	$(V)$(RM) $(GENERATED_H) $(GENERATED_SOURCES)

clean: clean_gen
	$(V)$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)

.cpp.o:
	$(V)$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<
