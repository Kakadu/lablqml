MLQT_OBJS= QPushButton.cmo  
MLQT_OBJS_OPT= QPushButton.cmx 

MLQT_ENUMS=
MLQT_ENUMS_OPT=

ML_OBJS = $(MLQT_ENUMS) qt.cmo $(MLQT_OBJS)  qtGui.cmo
ML_OBJS_OPT= $(MLQT_ENUMS_OPT) qt.cmx $(MLQT_OBJS_OPT) qtGui.cmx

CQT_ENUMS= 
CQT_OBJS= ml_QWidget.o ml_QPushButton.o ml_QLayout.o ml_qobject.o		ml_qapplication.o

C_OBJS= $(CQT_ENUMS) $(CQT_OBJS)

QTDEBUG_PATH=/home/kakadu/mand/prog/qt-everywhere-opensource-src-4.7.2/
QTDEBUG_INC=$(QTDEBUG_PATH)include
QTSYSTEM_INC=/usr/include/qt4
QTLIB=$(QTDEBUG_PATH)lib/
QTINC=$(QTDEBUG_INC)

COPTS = -I$(QTINC)/Qt -I$(QTINC)/QtCore \
	-I$(QTINC)/QtGui -I$(QTINC)/ # -I/usr/share/qt4/mkspecs/linux-g++   

CLIBS = -L$(QTLIB) -lQtGui -lQtCore -lpthread 
OCAMLC=ocamlc -g -annot 
OCAMLOPT=ocamlopt -g -annot -ccopt -O2
GCC=g++ -c -pipe -g -O2 -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED

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
	#ocamlmklib -o lablqt $(CLIBS) $(ML_OBJS_OPT) $(C_OBJS_OPT)

clean:
	rm -f *~ *.cm* *.[oa] *.so lablqt var2conv var2def *tags.[ch] *.annot

include .depend

