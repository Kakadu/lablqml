OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind c -g 
OCAMLOPT=ocamlfind opt -g
CPP_INCLUDES=-I.. -I/usr/include/qt4/ -I/usr/include/qt4/QtCore  
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

userslots.h:
	moc UserSlots.h > moc_UserSlots.cpp

main: userslots.h moc_UserSlots.o UserSlots.o UserSlots.cmo UserSlots_stubs.cmo main.cmo
	$(OCAMLC) -custom $(INCLUDES) lablqt.cma UserSlots.o moc_UserSlots.o \
		UserSlots.cmo UserSlots_stubs.cmo main.cmo -o $(OUTFILE)
	
clean:
	rm -f *.o *.cm[xio] $(OUTFILE) \
		moc_UserSlots.cpp UserSlots.h UserSlots_stubs.ml UserSlots.h UserSlots.cpp

include .depend

