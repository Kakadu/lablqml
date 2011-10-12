ML_OBJS=parse.cmo main.cmo 
ML_OBJS_OPT=parse.cmx main.cmx

PACKAGES=dynlink,str,threads,unix,core,tyxml,ocamlgraph,sexplib.syntax

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES) -syntax camlp4o
OCAMLC=ocamlfind ocamlc -g -vmthread -package $(PACKAGES) -syntax camlp4o 
OCAMLOPT=ocamlfind ocamlopt -g -thread -package $(PACKAGES) -syntax camlp4o

CMOS=$(CMOS_LIB) $(ML_OBJS)
CMXS=$(CMXS_LIB) $(ML_OBJS_OPT)

all: opt 

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx 

depend:
	$(OCAMLDEP)  *.ml *.mli > .depend

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo: 
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

opt: $(ML_OBJS_OPT)
	$(OCAMLOPT) camlp4lib.cmxa $(CMXS) -linkpkg  -o main.opt

byte: $(ML_OBJS)
	$(OCAMLC) $(CMOS) -linkpkg -o main.byte

clean:
	rm -f *~ *.cm* *.[oa] *.so  *tags.[ch] *.annot

include .depend

