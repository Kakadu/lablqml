INC= -I `ocamlc -where` -I . -I /usr/include/qt4 `pkg-config --cflags QtGui`
GCC=g++ -c -g -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT

.SUFFIXES: .ml .cpp .cmx .h
.PHONY: all

all: AA.h AA.o moc_AA.o cstubs.o enums.o classes.cmx main.cmx
	ocamlopt -cclib -lstdc++ -cclib -lQtGui -cclib -lQtCore cstubs.o enums.o AA.o moc_AA.o classes.cmx main.cmx -o main

.cpp.o:
	$(GCC) $(INC) -fpic $<

moc_%.cpp: %.h
	moc $< > $@

.ml.cmx:
	ocamlopt -c $<

clean:
	rm -f *.o *.cm[oix] moc_AA.cpp

test:
	$(MAKE) clean && $(MAKE) all && ./main
