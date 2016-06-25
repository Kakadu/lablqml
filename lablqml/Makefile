MAKE_PID := $(shell echo $$PPID)
JOB_FLAG := $(filter -j%, $(subst -j ,-j,$(shell ps T | grep "^\s*$(MAKE_PID).*$(MAKE)")))
JOBS     := $(subst -j,,$(JOB_FLAG))

$(message $(JOB))
$(message 1234123412341234)

OB=ocamlbuild -use-ocamlfind -classic-display
TARGETS=lablqml.cmxa lablqml.a QmlContext.cmi liblablqml_stubs.a dlllablqml_stubs.so
INSTALL_TARGETS=$(addprefix _build/,$(TARGETS))

space :=
space +=
join-with = $(subst $(space),$1,$(strip $2))

TEST_EXECS = test1 test2

COLON_SEPARATED_TESTS := ./$(call join-with,.native && ./,$(TEST_EXECS)).native
#$(warning $(COLON_SEPARATED_TESTS) )

.PHONY: tests_compile run_tests tests test bundle

all: lib bundle

lib:
	$(OB) liblablqml_stubs.a lablqml.cmxa

clean:
	$(RM) -r _build

# put all files to the single directory (to be accessible for ocamlfind)
BUNDLEDIR=_build/bundle/lablqml
define MAKE_BUNDLE_RULE
$(BUNDLEDIR)/$(1): $(1)
	cp $(1) $(BUNDLEDIR)
MAKE_BUNDLE_TARGETS += $(BUNDLEDIR)/$(1)
endef
$(foreach i,META $(INSTALL_TARGETS),$(eval $(call MAKE_BUNDLE_RULE,$(i)) ) )

$(BUNDLEDIR):
	mkdir -p $@


bundle: lib $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

tests_compile: lib
	$(OB) src_tests/$(call join-with,.native src_tests/,$(TEST_EXECS)).native

run_tests: tests_compile
	$(COLON_SEPARATED_TESTS)

tests: tests_compile run_tests
test: tests

install: bundle
	ocamlfind install lablqml META $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove lablqml
