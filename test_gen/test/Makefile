OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlc -g

all: main

main:
	$(OCAMLC) $(INCLUDES) lablqt.cma main.ml 
	
clean:
	rm -f *.cm[xio] $(OUTFILE)


