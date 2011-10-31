
.PHONY: build clean gen

build:
	ocamlbuild -j 0 -use-ocamlfind main.native

clean:
	ocamlbuild -clean

gen: build
	./main.native -xml -file ../for_new.xml
