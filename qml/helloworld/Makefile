OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package lablqml
PPX_QT=ppx_qt
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC
CC=g++
OUT=helloworld
QT_MODULES=Qt5Quick Qt5Widgets
CLINKLIBS = -cclib -lstdc++
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-L     $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-other $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -cclib , $(shell pkg-config --libs-only-l     $(QT_MODULES) ) )
GENERATED_FILES=

.PHONY: clean all generate app
.SUFFIXES: .cmx .o .ml .cpp .c

GENERATED_H=

all: generate $(OUT)

define MAKE_RULES
GENERATED_H += $(1).h
GENERATED_SOURCES += $(1)_c.cpp
GENERATED_CPP += $(1)_c.o
GENERATED_MOC += moc_$(1).o
GENERATED_CMX += $(1).cmx
$(1).h: $(1).ml
	ocamlopt -I `ocamlfind query lablqml` -c -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1).cmx: $(1).ml
	ocamlfind opt -I `ocamlfind query lablqml` -c -ppx "$(PPX_QT) -destdir . -nogen" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(CC) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

moc_$(1).cpp: $(1).h
	moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,controller,$(eval $(call MAKE_RULES,$(i)) ) )

generate: $(GENERATED_H)

$(OUT): $(GENERATED_CMX) program.cmx $(GENERATED_CPP) $(GENERATED_MOC)
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $^ -o $(OUT)

clean_gen:
	rm -f $(GENERATED_H) $(GENERATED_SOURCES)

clean: clean_gen
	rm -f *.cm[iox] *.o libcppstubs.a $(OUT)

.cpp.o:
	$(CC) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(OCAMLOPT) -c $<
