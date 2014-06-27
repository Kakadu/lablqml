OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package lablqml
#PPX_QT=ppx_qt
PPX_QT=../ppx_qt
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC
CC=g++
OUT=main
CLINKLIBS=-cclib -lstdc++ -ccopt -L$(shell qmake -query QT_INSTALL_LIBS) -cclib -lQt5Quick \
        -cclib -lQt5Gui -cclib -lQt5Qml -cclib -lQt5Widgets -cclib -lQt5Network -cclib -lQt5Core

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c


all: generate app

define BOZO
$(1).h: $(1).ml #$($(1):pp_/%=%)
	ocamlopt -I `ocamlfind query lablqml` -c -dsource -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(CC) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,controller,$(eval $(call BOZO,$(i)) ) )

generate: controller.h

cppstubs:
	ar cr  libcppstubs.a controller_c.o moc_controller.o

app: controller.cmx program.cmx moc_controller.cpp controller_c.o  moc_controller.o cppstubs
	$(OCAMLOPT) -linkpkg libcppstubs.a $(CLINKLIBS) controller.cmx program.cmx  \
	    -o $(OUT)

clean_gen:
	rm -f controller.h controller_c.cpp moc_controller.cpp

clean: clean_gen
	rm -f *.cm[iox] *.o libcppstubs.a $(OUT)

.cpp.o:
	$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(OCAMLOPT) -c $<