include ../Makefile.rule

OUT=signal_test.native

.PHONY: clean all generate app

all: generate $(OUT)

$(eval $(call MAKE_RULES,controller) )

generate: controller.h

libcppstubs.a:
	ar cr $@ controller_c.o moc_controller.o

$(OUT): $(GENERATED_CMX) $(GENERATED_OBJS) program.cmx libcppstubs.a
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg libcppstubs.a $(ML_LDFLAGS) $(ML_LDLIBS) $(CLINKLIBS) \
	controller.cmx program.cmx -o $(OUT)

.cpp.o:
	$(V)$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean::
	$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)
