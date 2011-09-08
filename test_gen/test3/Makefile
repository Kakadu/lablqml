OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind c -g
OCAMLOPT=ocamlfind opt -g

all: main

main:
	$(OCAMLC) $(INCLUDES) lablqt.cma main.ml -o $(OUTFILE)
	
clean:
	rm -f *.o *.cm[xio] $(OUTFILE)


