ML_OBJS =parser.cmo superIndex.cmo generators.cmo ocamlGenerator.cmo main.cmo 
ML_OBJS_OPT= 

#OCAML_INC=-I /usr/local/lib/ocaml/3.12.0/tyxml \
#	-I /usr/lib/ocaml/camlp4 -I /usr/lib/ocaml/ocamlgraph  

PACKAGES=dynlink,threads,unix,core,core_extended,tyxml,ocamlgraph,camlp4
OCAMLC=ocamlfind ocamlc -g -thread -package $(PACKAGES)
OCAMLOPT=ocamlopt -g -thread $(OCAML_INC)

CMOS_LIB=str.cma unix.cma threads.cma bigarray.cma nums.cma sexplib.cma bin_prot.cma res.cma \
        fieldslib.cma core.cma pcre.cma core_extended.cma \
	dynlink.cma camlp4lib.cma xmllexer.cmo simplexmlparser.cmo graph.cma

CMOS=$(CMOS_LIB) $(ML_OBJS)
#dynlink.cma str.cma unix.cma camlp4lib.cma xmllexer.cmo simplexmlparser.cmo graph.cma 

all:  lablqt

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx

depend:
	#$(OCAMLC) -i parser.ml > parser.mli
	#$(OCAMLC) -i generators.ml > generators.mli
	#$(OCAMLC) -i ocamlGenerator.ml > ocamlGenerator.mli
	ocamldep  *.ml *.mli > .depend

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

