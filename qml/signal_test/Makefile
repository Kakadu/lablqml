OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package lablqml
OCAMLC=ocamlfind c   -package lablqml
PPX_QT=ppx_qt.native
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC -I`ocamlc -where`
CC=g++
OUT=signal_test.native
CLINKLIBS=-cclib -lstdc++ -ccopt -L$(shell qmake -query QT_INSTALL_LIBS) -cclib -lQt5Quick \
        -cclib -lQt5Gui -cclib -lQt5Qml -cclib -lQt5Widgets -cclib -lQt5Network -cclib -lQt5Core

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c


all: generate $(OUT)

define BOZO
$(1).h: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLC) -c -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1).cmx: $(1).ml
	$(V)PATH=../../src/:$$$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(CXX) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,controller,$(eval $(call BOZO,$(i)) ) )

generate: controller.h

libcppstubs.a:
	ar cr $@ controller_c.o moc_controller.o

$(OUT): controller.cmx program.cmx moc_controller.cpp controller_c.o  moc_controller.o libcppstubs.a
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -linkpkg libcppstubs.a $(CLINKLIBS) controller.cmx program.cmx  \
	    -o $(OUT)

clean_gen:
	rm -f controller.h controller_c.cpp moc_controller.cpp

clean: clean_gen
	rm -f *.cm[iox] *.o libcppstubs.a $(OUT)

.cpp.o:
	$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(V)PATH=../../src/:$$PATH OCAMLPATH=../../lablqml/_build/bundle \
	$(OCAMLOPT) -c $<
