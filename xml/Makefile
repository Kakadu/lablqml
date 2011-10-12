ML_OBJS=parser.cmo superIndex.cmo generators.cmo cppGenerator.cmo ocamlGenerator.cmo main.cmo 
ML_OBJS_OPT=parser.cmx superIndex.cmx generators.cmx cppGenerator.cmx ocamlGenerator.cmx main.cmx

PACKAGES=dynlink,str,threads,unix,core,tyxml,ocamlgraph,sexplib.syntax

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES) -syntax camlp4o
OCAMLC=ocamlfind ocamlc -g -vmthread -package $(PACKAGES) -syntax camlp4o 
OCAMLOPT=ocamlfind ocamlopt -g -thread -package $(PACKAGES) -syntax camlp4o

#CMOS_LIB=str.cma unix.cma threads.cma bigarray.cma nums.cma sexplib.cma bin_prot.cma res.cma \
        fieldslib.cma core.cma pcre.cma core_extended.cma \
	dynlink.cma camlp4lib.cma xmllexer.cmo simplexmlparser.cmo graph.cma 

#CMXS_LIB=str.cmxa unix.cmxa threads.cmxa bigarray.cmxa nums.cmxa sexplib.cmxa bin_prot.cmxa res.cmxa \
        fieldslib.cmxa core.cmxa pcre.cmxa core_extended.cmxa \
	dynlink.cmxa camlp4lib.cmxa xmllexer.cmx simplexmlparser.cmx graph.cmxa 

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
	$(OCAMLOPT) camlp4lib.cmxa xmllexer.cmx simplexmlparser.cmx $(CMXS) -linkpkg  -o main.opt

byte: $(ML_OBJS)
	$(OCAMLC) $(CMOS) -linkpkg -o main

clean:
	rm -f *~ *.cm* *.[oa] *.so  *tags.[ch] *.annot

include .depend

