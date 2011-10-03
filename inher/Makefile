INC=-I ../simple_signals -I `ocamlc -where` -I .
GCC=g++ -c

.SUFFIXES: .ml .cpp .mli .cmx

.cpp.o:
	$(GCC) $(INC) -fpic $<

.ml.cmx:
	ocamlopt -c $<

all: A.o AA.o classes.cmx

clean:
	rm -f *.o *.cm[oix]

