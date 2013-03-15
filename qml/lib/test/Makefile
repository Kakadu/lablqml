MOC=$$QT5/bin/moc
CC=g++ -fPIC -std=c++0x -I$$QT5_ROOT/qtbase/include -O2 -I../_build -I`ocamlc -where`
CLINK=g++ -L$$QT5_ROOT/qtbase/lib -lQt5Core -lQt5Qml -lQt5Quick -lQt5Gui
OUT=main
GEN_CMX=A.cmx
GEN_CPP=A_c.o moc_A_c.o
GEN_MOC=A_c.cpp
.SUFFIXES: .cpp .h .o .ml .cmx .cmo .cmi
.PHONY: all clean

all: gen_stubs $(GEN_CMX) program.cmx library_code $(GEN_MOC) $(GEN_CPP) main.o
	$(CLINK) \
	../_build/stubs.o camlcode.o $(GEN_CPP) main.o -L"`ocamlc -where`" $(NATIVECCLIBS) -lasmrun \
	-o $(OUT)

library_code:
	ocamlfind opt -output-obj -dstartup -I ../_build \
	$(GEN_CMX) ../_build/qml_wrap.cmxa program.cmx  -linkall -o camlcode.o #-verbose

gen_stubs: input_yaml
	./mocml -qml_wrap

moc_%.cpp: %.h
	$(MOC) $< > $@

.cpp.o:
	$(CC) -c $< -I.

.ml.cmx:
	ocamlfind opt -I ../_build -c $<

clean:
	rm *.o *.cm[oiax] *.cmxa *.o.startup.s -f

-include  $(shell ocamlc -where)/Makefile.config
