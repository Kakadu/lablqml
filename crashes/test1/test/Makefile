OCAML=`ocamlc -where`
INCLUDES=-I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind opt -g

all: main

main:
	$(OCAMLC) $(INCLUDES) lablqt.cmxa main.ml -o $(OUTFILE)
	
clean:
	rm -f *.cm[xio] $(OUTFILE)


