OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package lablqml
PPX_QT=ppx_qt
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC
OUT=demo1
QT_MODULES=Qt5Quick Qt5Widgets
CLINKLIBS = -cclib -lstdc++
CLINKLIBS +=$(addprefix -ccopt , $(shell pkg-config --libs-only-L     $(QT_MODULES) ) )
CLINKLIBS +=$(addprefix -ccopt , $(shell pkg-config --libs-only-other $(QT_MODULES) ) )
CLINKLIBS +=$(addprefix -cclib , $(shell pkg-config --libs-only-l     $(QT_MODULES) ) )

#$(warning $(CLINKLIBS)  )

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c

all: generate $(OUT)

define MAKE_RULES
$(1).h: $(1).ml
	ocamlopt -I `ocamlfind query lablqml` -c -dsource -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(CXX) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,controller,$(eval $(call MAKE_RULES,$(i)) ) )

generate: controller.h

libcppstubs.a:
	ar cr $@ controller_c.o moc_controller.o

$(OUT): controller.cmx program.cmx moc_controller.cpp controller_c.o  moc_controller.o libcppstubs.a
	$(OCAMLOPT) -linkpkg libcppstubs.a $(CLINKLIBS) controller.cmx program.cmx  \
	    -o $(OUT)

clean_gen:
	rm -f controller.h controller_c.cpp moc_controller.cpp

clean: clean_gen
	rm -f *.cm[iox] *.o libcppstubs.a $(OUT)

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(OCAMLOPT) -c $<
