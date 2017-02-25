include ../Makefile.rule

#export PATH=$(shell echo ${PATH}):../../src
#export OCAML_PATH=/home/kakadu/lablqt/lablqml/_build/bundle
# WTF. We can't export variable to subshells in make

OUT=helloworld
GENERATED_FILES=

.DEFAULT_GOAL: all
.PHONY: clean all generate app

GENERATED_H=

all: generate $(OUT)
TO_GENERATE=controller

$(eval $(call MAKE_RULES,$(TO_GENERATE) ) )

generate: $(GENERATED_H)

$(OUT): $(GENERATED_CMX) program.cmx $(GENERATED_CPP) $(GENERATED_MOC)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $(ML_LDFLAGS) $(ML_LDLIBS) $^ -o $(OUT)

clean::
	$(V)$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -c $<
