.PHONY: all generator lib demo rundemo test watch doc celan

all: lib generator
	dune build -p lablqml

generator:
	dune build ppx/pp/pp_qt.exe
lib:
	dune build lib/lablqml.a lib/dlllablqml_stubs.so

test: demo
demo: all
	PATH=`pwd`/_build/default/ppx/ppxext:$(PATH) \
	dune build dune_test/ui/Root.qml \
		dune_test/moc_controller_c.cpp dune_test/program.exe

rundemo:
	PATH=`pwd`/_build/default/ppx/ppxext:$(PATH) \
	dune exec -- dune_test/program.exe

celan: clean
clean:
	$(RM) -r _build
	$(RM) -r doc/_build

uninstall:
	dune build @install
	dune uninstall

install:
	dune build @install
	dune install

watch:
	dune build -w

doc:
	cd doc && sphinx-build . _build

deps:
	opam install --yes ppx_show ppx_string_interpolation ppxlib dune-configurator dune ppx_inline_test
