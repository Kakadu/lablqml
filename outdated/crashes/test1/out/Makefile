ML_MODULES=stubs.cmo creators.cmo classes.cmo 
ML_MODULES_OPT=stubs.cmx creators.cmx classes.cmx  

OCAMLC=ocamlc.opt -g
OCAMLOPT=ocamlopt.opt -g

INC=-I ..
.SUFFIXES: .ml .mli .cmi .cmx .cmo 

.ml.cmo:
	$(OCAMLC)   $(INC) -c $<

.ml.cmx:
	$(OCAMLOPT) $(INC) -c $<

.mli.cmi:
	$(OCAMLC)  $(INC) -c $<

all: byte opt

depend:
	ocamldep $(INC) *.ml *.mli > .depend

byte: $(ML_MODULES)

opt:  $(ML_MODULES_OPT)

.PHONY: all clean opt byte

include .depend

clean:
	rm -f *.cm[iox] *.o

