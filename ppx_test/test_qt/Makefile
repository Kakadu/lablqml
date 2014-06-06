all: ppx

src:
	ocamlc -c -dparsetree def.ml

ppx:
	ocamlc -c -dsource -ppx ../ppx_qt.native def.ml
