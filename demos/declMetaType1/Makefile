include ../Makefile.rule

#export PATH=$(shell echo ${PATH}):../../src
#export OCAML_PATH=/home/kakadu/lablqt/lablqml/_build/bundle
# WTF. We can't export variable to subshells in make

OUT=declMetaType1
GENERATED_FILES=

.DEFAULT_GOAL: all
.PHONY: clean all generate app

GENERATED_H=
OCAML_OPT_FLAGS+=-w @5
OCAML_COMMON_FLAGS+=-g
override CXXFLAGS+=-g

all: generate $(OUT)
TO_GENERATE=demoClass1

$(eval $(call MAKE_RULES,$(TO_GENERATE) ) )

generate: $(GENERATED_H)

$(OUT): $(GENERATED_CMX) program.cmx $(GENERATED_MOC) $(GENERATED_CPP)
	$(V)PATH=../../ppx:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $(ML_LDFLAGS) $(ML_LDLIBS) $^ -o $(OUT)

clean::
	$(V)$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)

.ml.cmx:
	$(V)PATH=../../ppx/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -c $<
