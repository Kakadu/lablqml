OCAML=`ocamlc -where`
INCLUDES=-I . -I ./.. -I ./../out
OUTFILE=test
OCAMLC=ocamlfind opt -g

all: opt byte

opt:
	$(OCAMLC) $(INCLUDES) lablqt.cmxa main.ml -o $(OUTFILE)

byte:
	ocamlfind c -custom -g -I . -I ./.. -I ./../out lablqt.cma main.ml -o test.byte

clean:
	rm -f *.cm[xio] $(OUTFILE)


