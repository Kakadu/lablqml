ML_OBJS =parser.cmo superIndex.cmo generators.cmo cppGenerator.cmo ocamlGenerator.cmo main.cmo 
ML_OBJS_OPT= 

PACKAGES=dynlink,threads,unix,core,core_extended,tyxml,ocamlgraph,camlp4
PP=-pp "camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sexp_conv.cmo"
OCAMLC=ocamlfind ocamlc -g -thread -package $(PACKAGES) $(PP) 

OCAMLOPT=ocamlopt -g -thread $(OCAML_INC)

CMOS_LIB=str.cma unix.cma threads.cma bigarray.cma nums.cma sexplib.cma bin_prot.cma res.cma \
        fieldslib.cma core.cma pcre.cma core_extended.cma \
	dynlink.cma camlp4lib.cma xmllexer.cmo simplexmlparser.cmo graph.cma 

CMOS=$(CMOS_LIB) $(ML_OBJS)

all:  lablqt

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx

depend:
	ocamldep $(PP) *.ml *.mli > .depend

.ml.cmx:
	$(OCAMLOPT) -c $<
.ml.cmo: 
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<

lablqt: lablqt.cma 

lablqt.cma: $(ML_OBJS)
	$(OCAMLC) $(CMOS) $(ML_OBJS) -o main

clean:
	rm -f *~ *.cm* *.[oa] *.so  *tags.[ch] *.annot

include .depend

