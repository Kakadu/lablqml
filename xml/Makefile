
.PHONY: build clean

build: byte

all: byte native

native:
	ocamlbuild -j 0 -use-ocamlfind main.native xmltool.native

byte:
	ocamlbuild -j 0 -use-ocamlfind main.byte xmltool.byte

clean:
	ocamlbuild -clean

