INC=-I ../simple_signals -I `ocamlc -where` -I .
GCC=g++ -c -g -O0 -fno-inline

.SUFFIXES: .ml .cpp .mli .cmx

.cpp.o:
	$(GCC) $(INC) -fpic $<

.ml.cmx:
	ocamlopt -c $<

all: A.o WrapperClass.o AA.o classes.cmx main.cmx
	ocamlopt -cclib -lstdc++ A.o WrapperClass.o AA.o classes.cmx main.cmx -o main

clean:
	rm -f *.o *.cm[oix]

test:
	$(MAKE) all && $(MAKE) clean && ./main
