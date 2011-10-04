OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I .
OUTFILE=test
OCAMLC=ocamlfind c -g -vmthread -package lablqt
OCAMLOPT=ocamlfind opt -thread -g -package lablqt
CPP_INCLUDES=-I.. `pkg-config --cflags QtOpenGL` -I`ocamlc -where` 
GCC=g++ -g $(CPP_INCLUDES) 
all: main

.SUFFIXES: .cpp .ml .cmo .o

.cpp.o:
	$(GCC) -c $<

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $<

depend:
	./mocml input
	ocamldep *.ml *.mli > .depend
	moc UserSlots.h > moc_UserSlots.cpp

userslots.h:

main: userslots.h moc_UserSlots.o UserSlots.o UserSlots.cmo UserSlots_stubs.cmo main.cmo
	$(OCAMLC) -custom $(INCLUDES) lablqt.cma UserSlots.o moc_UserSlots.o \
		UserSlots.cmo UserSlots_stubs.cmo main.cmo -o $(OUTFILE)
	
clean:
	rm -f *.o *.cm[xio] $(OUTFILE) \
		moc_UserSlots.cpp UserSlots.h UserSlots_stubs.ml UserSlots.h UserSlots.cpp

include .depend

