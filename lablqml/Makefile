OB=ocamlbuild -use-ocamlfind -classic-display
TARGETS=lablqml.cmxa lablqml.a QmlContext.cmi liblablqml_stubs.a dlllablqml_stubs.so
INSTALL_TARGETS=$(addprefix _build/,$(TARGETS))

space :=
space +=
join-with = $(subst $(space),$1,$(strip $2))

TEST_EXECS = test1 test2

COLON_SEPARATED_TESTS := ./$(call join-with,.native && ./,$(TEST_EXECS)).native
#$(warning $(COLON_SEPARATED_TESTS) )

.PHONY: tests_compile run_tests tests test

all: lib

lib:
	$(OB) liblablqml_stubs.a lablqml.cmxa

clean:
	$(RM) -r _build


tests_compile: lib
	$(OB) src_tests/$(call join-with,.native src_tests/,$(TEST_EXECS)).native

run_tests: tests_compile
	$(COLON_SEPARATED_TESTS)

tests: tests_compile run_tests
test: tests

install:
	ocamlfind install lablqml META $(INSTALL_TARGETS)

uninstall:
	ocamlfind remove lablqml
