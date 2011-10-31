OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind c -g

all: main

main:
	$(OCAMLC) $(INCLUDES) lablqt.cma main.ml -o $(OUTFILE)
	
clean:
	rm -f *.cm[xio] $(OUTFILE)


