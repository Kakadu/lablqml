qtgcc=g++ -g `pkg-config --cflags QtDeclarative` -I`ocamlfind c -where`
asdf=-c -m64 -pipe -g -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_DECLARATIVE_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I/usr/share/qt4/mkspecs/linux-g++-64 -I. -I/usr/include/qt4/QtCore -I/usr/include/qt4/QtGui -I/usr/include/qt4/QtDeclarative -I/usr/include/qt4 -I.
mocs=moc_gamemap.cpp
cppobjs=gamemap.o moc_gamemap.o main.o
mlobjs=stubs.o
ocamlobjs=ocaml/camlcode.o
out=main
-include $(shell ocamlc -where)/Makefile.config

.SUFFIXES: .cpp .h .o

.PHONY: all kamlo

all: $(mocs) $(cppobjs) kamlo
	$(qtgcc) $(cppobjs) $(ocamlobjs) `pkg-config --libs QtDeclarative` \
	-L"`ocamlfind c -where`" $(NATIVECCLIBS) -lasmrun \
	-o $(out)

kamlo:
	$(MAKE) -C ocaml

moc_%.cpp: %.h
	moc $< > $@

.cpp.o:
	$(qtgcc) -c $<

clean:
	rm moc_* *.o $(out) -f
	$(MAKE) -s --no-print-directory -C ocaml clean
