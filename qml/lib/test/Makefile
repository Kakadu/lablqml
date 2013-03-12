CC=g++ -std=c++0x -I$$QT5_ROOT/qtbase/include -O2
OUT=main
all:
	ocamlfind opt -output-obj -dstartup ../_build/QmlContext.cmx  \
	-linkall -verbose -o camlcode.o
	$(CC) -fPIC -c main.cpp -I`ocamlc -where`
	g++ -L$$QT5_ROOT/qtbase/lib -lQt5Core -lQt5Quick -lQt5Gui \
	../_build/stubs.o camlcode.o  main.o -L"`ocamlc -where`" $(NATIVECCLIBS) -lasmrun \
	-o $(OUT)

clean:
	rm *.o *.cm[oiax] *.cmxa *.o.startup.s -f

-include  $(shell ocamlc -where)/Makefile.config
