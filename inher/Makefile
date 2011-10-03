INC=-I ../simple_signals -I `ocamlc -where` -I .
GCC=g++ -c


.cpp.o:
	$(GCC) $(INC) -fpic $<

all: A.o AA.o

clean:
	rm -f *.o *.cm[oix]

