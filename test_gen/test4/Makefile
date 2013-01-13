OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind c -g 
OCAMLOPT=ocamlfind opt -g
CPP_INCLUDES=-I.. `pkg-config --cflags QtOpenGL` -I`ocamlc -where` 
GCC=g++ -g $(CPP_INCLUDES) 
all: main

main:
	./mocml -qtgui input
	moc UserSlots.h > moc_UserSlots.cpp
	$(GCC) -c moc_UserSlots.cpp 
	$(GCC) -c UserSlots.cpp 
	$(GCC) -c UserSlots_helpers.cpp
	$(OCAMLC) $(INCLUDES) -c UserSlots.ml
	$(OCAMLC) $(INCLUDES) -c UserSlots_stubs.ml
	$(OCAMLC) $(INCLUDES) -c main.ml 
	$(OCAMLC) -custom $(INCLUDES) lablqt.cma UserSlots_helpers.o UserSlots.o moc_UserSlots.o \
		UserSlots.cmo UserSlots_stubs.cmo main.cmo -o $(OUTFILE)
	
clean:
	rm -f *.o *.cm[xio] $(OUTFILE) \
		moc_UserSlots.cpp UserSlots.h UserSlots_stubs.ml UserSlots.h UserSlots.cpp


