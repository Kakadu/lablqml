INC= -I `ocamlc -where` -I . -I /usr/include/qt4 `pkg-config --cflags QtGui`
GCC=g++ -c -g -O0 -fno-inline 

.SUFFIXES: .ml .cpp .cmx .h
.PHONY: all

all: cstubs.o classes.cmx main.cmx
	ocamlopt -cclib -lstdc++ -cclib -lQtGui -cclib -lQtCore cstubs.o classes.cmx main.cmx -o main

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
