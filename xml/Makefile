ML_OBJS =parser.cmo generators.cmo main.cmo 
ML_OBJS_OPT= 


OCAML_INC=-I /usr/local/lib/ocaml/3.12.0/tyxml \
	-I /usr/lib/ocaml/camlp4  

OCAMLC=ocamlfind ocamlc -g $(OCAML_INC)
OCAMLOPT=ocamlopt -g  $(OCAML_INC)
CMOS= dynlink.cma str.cma unix.cma camlp4lib.cma xmllexer.cmo simplexmlparser.cmo

all:  lablqt

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx

depend:
	$(OCAMLC) -i parser.ml > parser.mli
	$(OCAMLC) -i generators.ml > generators.mli
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

