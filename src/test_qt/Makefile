all: ppx

src:
	ocamlc -c -dparsetree def.ml

ppx:
	ocamlc -c -dsource -ppx "../ppx_qt.native -nocpp" def.ml && echo "PRINTING SOURCE" && \
	ocamlc -c -dsource -ppx ../ppx_qt.native def.ml
	g++ -std=c++0x `pkg-config --cflags Qt5Core` -fPIC -c controller.cpp historyModel.cpp





