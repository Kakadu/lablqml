OCAMLC=ocamlc
OCAMLOPT=ocamlfind opt -package lablqml
PPX_QT=ppx_qt
CXXFLAGS=`pkg-config --cflags Qt5Quick` -fPIC
OUT=demo3
QT_MODULES=Qt5Quick Qt5Widgets
CLINKLIBS = -cclib -lstdc++
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-L     $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -ccopt , $(shell pkg-config --libs-only-other $(QT_MODULES) ) )
CLINKLIBS+= $(addprefix -cclib , $(shell pkg-config --libs-only-l     $(QT_MODULES) ) )
GENERATED_FILES=

# prefixes for files where PPX syntax extension is used
# Add more files there when needed
FILES_FOR_PPX=controller

.PHONY: clean all generate
.SUFFIXES: .cmx .o .ml .cpp .c

all: generate $(OUT)

define MAKE_RULES

GENERATED_FILES+=$(1).h $(1)_c.cpp
$(1).h: $(1).ml
	ocamlopt -I `ocamlfind query lablqml` -c -dsource -ppx "$(PPX_QT) -destdir . -ext cpp" $(1).ml

$(1)_c.o: $(1)_c.cpp
	$(CXX) $(CXXFLAGS) -c $(1)_c.cpp -o $(1)_c.o

GENERATED_FILES+=moc_$(1).cpp
moc_$(1).cpp: $(1).h
	moc $(1).h -o moc_$(1).cpp

endef
$(foreach i,$(FILES_FOR_PPX),$(eval $(call MAKE_RULES,$(i)) ) )

generate: $(addsuffix .h,$(FILES_FOR_PPX))

$(OUT): controller.cmx program.cmx controller_c.o moc_controller.o
	$(OCAMLOPT) -linkpkg $(CLINKLIBS) $^ -o $(OUT)

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *.o libcppstubs.a $(GENERATED_FILES) $(OUT)
