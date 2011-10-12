ML_OBJS=simplexmlwriter.cmo main.cmo 
ML_OBJS_OPT=simplexmlwriter.cmx main.cmx

PACKAGES=dynlink,str,tyxml,ocamlgraph,sexplib,core

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES)
OCAMLC=ocamlfind ocamlc -g -package $(PACKAGES) 
OCAMLOPT=ocamlfind ocamlopt -g -thread -package $(PACKAGES) 
INC=-I `ocamlc -where`/camlp4
CMOS=$(CMOS_LIB) $(ML_OBJS)
CMXS=$(CMXS_LIB) $(ML_OBJS_OPT)

all: opt 

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx 

depend:
	$(OCAMLDEP)  *.ml *.mli > .depend

.ml.cmx:
	$(OCAMLOPT) -I ../xml -c $<

.ml.cmo: 
	$(OCAMLC) $(INC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

opt: $(ML_OBJS_OPT)
	$(OCAMLOPT) $(INC) ../xml/parser.cmx ../xml/superIndex.cmx \
		camlp4lib.cmxa xmllexer.cmx simplexmlparser.cmx  $(CMXS) -linkpkg -o main.opt

byte: $(ML_OBJS)
	$(OCAMLC) -I `ocamlc -where`/camlp4 parser.cmo ../xml/superIndex.cmo \
		camlp4lib.cma xmllexer.cmo simplexmlparser.cmo \
		$(CMOS) -linkpkg -o main.byte

clean:
	rm -f *~ *.cm* *.[oa] *.so  *tags.[ch] *.annot

main.cmo: simplexmlwriter.cmo
main.cmx: simplexmlwriter.cmx
simplexmlwriter.cmo:
simplexmlwriter.cmx:
