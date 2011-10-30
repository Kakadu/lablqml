
.PHONY: build clean gen

build: main.native

main.native:
	ocamlbuild -j 0 -use-ocamlfind main.native

clean:
	ocamlbuild -clean

gen: main.native
	./main.native -xml -file ../for_new.xml
