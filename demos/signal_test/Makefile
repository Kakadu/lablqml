OCAMLOPT=ocamlfind opt -package lablqml
OCAMLC=ocamlfind c   -package lablqml
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC -I`ocamlc -where`
CC=g++
OUT=signal_test.native
CLINKLIBS=-cclib -lstdc++ -ccopt -L$(shell qmake -query QT_INSTALL_LIBS) -cclib -lQt5Quick \
        -cclib -lQt5Gui -cclib -lQt5Qml -cclib -lQt5Widgets -cclib -lQt5Network -cclib -lQt5Core

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c

all: generate $(OUT)

.SUFFIXES: .cmx .o .ml .cpp .c
ifeq ($(VERBOSE),1)
V=
else
V=@
endif

include ../Makefile.rule
$(foreach i,controller,$(eval $(call MAKE_RULES,$(i)) ) )

generate: controller.h

libcppstubs.a:
	ar cr $@ controller_c.o moc_controller.o

$(OUT): $(GENERATED_CMX) $(GENERATED_OBJS) program.cmx libcppstubs.a
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg libcppstubs.a $(CLINKLIBS) controller.cmx program.cmx  \
	    -o $(OUT)

.cpp.o:
	$(V)$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<

clean_gen:
	$(RM) controller.h controller_c.cpp moc_controller.cpp

clean: clean_gen
	$(RM) *.cm[iox] *.o libcppstubs.a $(OUT)
