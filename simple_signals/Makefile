MLQT_OBJS= QPushButton.cmo  

MLQT_OBJS_OPT= QPushButton.cmx 

MLQT_ENUMS=

MLQT_ENUMS_OPT=

ML_OBJS = $(MLQT_ENUMS) qt.cmo $(MLQT_OBJS)  qtGui.cmo
ML_OBJS_OPT= $(MLQT_ENUMS_OPT) qt.cmx $(MLQT_OBJS_OPT) qtGui.cmx

CQT_ENUMS= 

CQT_OBJS=  \
	  ml_QPushButton.o 	  ml_qobject.o		ml_qapplication.o

C_OBJS= $(CQT_ENUMS) $(CQT_OBJS)
QTDEBUG_PATH=/home/kakadu/prog/qt-all-opensource-src-4.5.3/include
QTSYSTEM_PATH=/usr/include/qt4
QTPATH=$(QTSYSTEM_PATH)

COPTS = -I/usr/share/qt4/mkspecs/linux-g++   -I$(QTPATH)/Qt -I$(QTPATH)/QtCore \
	-I$(QTPATH)/QtGui -I$(QTPATH)/

CLIBS = -L/usr/lib -lQtGui -lQtCore -lpthread -L/usr/local/lib -L/usr/lib/
OCAMLC=ocamlc -g 
OCAMLOPT=ocamlopt -g
GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED

all:  lablqt

.SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx

.cpp.o:
	$(GCC) -c -I`ocamlc -where` $(COPTS) -fpic $<
.ml.cmx:
	$(OCAMLOPT) -c $<
.ml.cmo: 
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<

lablqt: lablqt.cma lablqt.cmxa

depend:
	ocamldep -pp camlp4o *.ml *.mli > .depend

lablqt.cma: $(C_OBJS) $(ML_OBJS)
	ocamlmklib -o lablqt $(CLIBS) $(ML_OBJS) $(C_OBJS) 

lablqt.cmxa: $(C_OBJS_OPT) $(ML_OBJS_OPT)
	ocamlmklib -o lablqt $(CLIBS) $(ML_OBJS_OPT) $(C_OBJS_OPT)

clean:
	rm -f *~ *.cm* *.[oa] *.so lablqt var2conv var2def *tags.[ch] *.annot

include .depend
