all: ppx

src:
	ocamlc -c -dparsetree def.ml

ppx:
	ocamlc -c -dsource -ppx "../ppx_qt.native -nocpp" def.ml && echo "PRINTING SOURCE" && \
	ocamlc -c -dsource -ppx "../ppx_qt.native -destdir ." def.ml
	g++ -std=c++0x `pkg-config --cflags Qt5Core` -Dprotected=public -fPIC -c \
	controller_c.c historyModel_c.c

clean:
	rm -fr *.o *.cm[oi] controller.c controller.cpp controller.h \
	historyModel.cpp historyModel.h historyModel_c.c



