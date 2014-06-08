all: ppx

src:
	ocamlc -c -dparsetree def.ml

ppx:
	ocamlc -c -ppx "../ppx_qt.native -nocpp" def.ml && echo "PRINTING SOURCE" && \
	ocamlc -c -dsource -ppx ../ppx_qt.native def.ml
	g++ `pkg-config --cflags Qt5Core` -fPIC -c controller.cpp





