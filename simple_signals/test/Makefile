OCAML=`ocamlc -where`
INCLUDES=-I /usr/lib/ocaml -I . -I ./..

all: main

OCAMLC=ocamlopt -g
main:
	$(OCAMLC) -c -pp camlp4o $(INCLUDES) main.ml 
	$(OCAMLC) $(INCLUDES) -cclib -lstdc++ lablqt.cmxa main.cmx -o test

clean:
	        rm -f *.cm[xio] 


