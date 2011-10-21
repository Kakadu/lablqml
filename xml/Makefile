ML_OBJS=parser.cmo superIndex.cmo generators.cmo filter.cmo cppGenerator.cmo ocamlGenerator.cmo main.cmo 
ML_OBJS_OPT=parser.cmx superIndex.cmx generators.cmx filter.cmx cppGenerator.cmx ocamlGenerator.cmx main.cmx

PACKAGES=dynlink,str,threads,unix,core,tyxml,type-conv,ocamlgraph,sexplib.syntax#,sexplib

OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES) -syntax camlp4o
OCAMLC=ocamlfind c -g -vmthread -package $(PACKAGES) -pp "camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma" -I `ocamlfind query type-conv` -I `ocamlfind query sexplib`
OCAMLOPT=ocamlfind opt -g -thread -package $(PACKAGES) -syntax camlp4o # -pp "camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` pa_type_conv.cma pa_sexp_conv.cma" -I `ocamlfind query type-conv` -I `ocamlfind query sexplib`

CMOS=$(CMOS_LIB) $(ML_OBJS)
CMXS=$(CMXS_LIB) $(ML_OBJS_OPT)

all: opt 

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx 

depend:
	$(OCAMLDEP)  *.ml *.mli > .depend

.ml.cmx:
	$(OCAMLOPT) -c $<

#.ml.cmo: 
#	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLOPT) -c $<

opt: $(ML_OBJS_OPT)
	$(OCAMLOPT) camlp4lib.cmxa xmllexer.cmx simplexmlparser.cmx $(CMXS) -linkpkg  -o main.opt

#byte: $(ML_OBJS)
#	$(OCAMLC) $(CMOS) -linkpkg -o main

clean:
	rm -f *~ *.cm[oixa] *.[oa] *.so  *tags.[ch] *.annot

include .depend

