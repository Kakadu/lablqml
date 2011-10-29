INC= -I `ocamlc -where` -I . -I /usr/include/qt4
GCC=g++ -c -g -O0 -fno-inline 

.SUFFIXES: .ml .cpp .mli .cmx

.cpp.o:
	$(GCC) $(INC) -fpic $<

.ml.cmx:
	ocamlopt -c $<

all: cstubs.o enums.o AA.o classes.cmx main.cmx
	ocamlopt -cclib -lstdc++ -cclib -lQtGui -cclib -lQtCore cstubs.o enums.o AA.o classes.cmx main.cmx -o main

clean:
	rm -f *.o *.cm[oix]

test:
	$(MAKE) clean && $(MAKE) all && ./main
