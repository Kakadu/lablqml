INC= -I `ocamlc -where` -I . -I /usr/include/qt4
GCC=g++ -c -g -O0 -fno-inline 

.SUFFIXES: .ml .cpp .mli .cmx
.PHONY: all

all: cstubs.o enums.o moc2 AA.o moc_AA.cpp classes.cmx main.cmx
	ocamlopt -cclib -lstdc++ -cclib -lQtGui -cclib -lQtCore cstubs.o enums.o AA.o moc_AA.o classes.cmx main.cmx -o main

.cpp.o:
	$(GCC) $(INC) -fpic $<

moc1: 
	moc AA.h > moc_AA.cpp

moc2: moc1
	$(GCC) $(INC) -fpic moc_AA.cpp

.ml.cmx:
	ocamlopt -c $<


clean:
	rm -f *.o *.cm[oix] moc_AA.cpp

test:
	$(MAKE) clean && $(MAKE) all && ./main
