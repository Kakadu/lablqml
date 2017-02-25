# In this makefile we make a library from OCaml code.
# It is not required in this concrete case but can be useful if we do
# start-up from C

include ../Makefile.rule

OCAMLFIND_PACKAGES+=compiler-libs.common str threads
OCAML_OPT_FLAGS+=-thread
OCAML_C_FLAGS+=-vmhread
OCAML_COMMON_FLAGS+=-g

OUT=test

.DEFAULT_GOAL: all
.PHONY: clean all generate app

all: generate $(OUT)

GEN_TARGETS=controller abstractModel dataItem
$(call MAKE_RULES,$(GEN_TARGETS))

generate: $(GENERATED_H)

OUT=test
CMX=helpers.cmx tree.cmx S.cmx Richify.cmx program.cmx
CMX_TEST=Test.cmx

.SUFFIXES: .c .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all depend clean

$(OUT): generate $(GENERATED_CMX) $(CMX) $(GENERATED_MOC) $(GENERATED_CPP)
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -linkpkg $(GENERATED_CPP) $(GENERATED_MOC) \
	$(GENERATED_CMX) $(CMX) $(ML_LDFLAGS) $(ML_LDLIBS) $(CLINKLIBS)  \
	-o $(OUT)

library_code:
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lib/_build/bundle \
	$(V)$(OCAMLOPT) -output-obj -dstartup $(GEN_CMX) $(CMX) -linkall -o camlcode.o

OCAML_MAJOR=$(shell ocamlc -version | cut -d . -f 1)
OCAML_MINOR=$(shell ocamlc -version | cut -d . -f 2)

Richify.ml: Richify.cppo.ml
	cppo -D 'OCAML_MAJOR $(OCAML_MAJOR)' -D 'OCAML_MINOR $(OCAML_MINOR)' \
		$< -o $@

.ml.cmx:
	$(V)OCAMLPATH=../../lib/_build/bundle \
	$(OCAMLOPT) -c $<

clean::
	$(RM) *.o *.cm[oiax] Richify.ml *.cmxa $(OUT)
