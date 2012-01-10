qtgcc=g++ `pkg-config --cflags QtDeclarative` -I`ocamlfind c -where`
asdf=-c -m64 -pipe -g -Wall -W -D_REENTRANT -DQT_WEBKIT -DQT_DECLARATIVE_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I/usr/share/qt4/mkspecs/linux-g++-64 -I. -I/usr/include/qt4/QtCore -I/usr/include/qt4/QtGui -I/usr/include/qt4/QtDeclarative -I/usr/include/qt4 -I.
mocs=moc_gamemap.cpp
cppobjs=gamemap.o moc_gamemap.o
mlobjs=stubs.o
ocamlobjs=ocaml/camlcode.o
out=main
-include $(shell ocamlc -where)/Makefile.config

.SUFFIXES: .cpp .h

all: $(mocs) $(cppobjs) ocaml main

ocaml:
	$(MAKE) -C ocaml

main:
	$(qtgcc) main.cpp $(cppobjs) $(ocamlobjs) `pkg-config --libs QtDeclarative` \
	-L"`ocamlfind c -where`" $(NATIVECCLIBS) -lasmrun \
	-o $(out)


moc_%.cpp: %.h
	moc $< > $@

.cpp.o:
	$(qtgcc) -c $<

clean:
	rm moc_* *.o $(out) -f
