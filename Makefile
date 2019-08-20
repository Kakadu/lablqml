.PHONY: all generator lib demo rundemo test
all: lib generator

generator:
	dune build --only-packages ppx_qt
lib:
	dune build --only-packages lablqml

test: demo
demo: all
	PATH=`pwd`/_build/default/ppx/ppxext:$(PATH) \
	dune build dune_test/ui/Root.qml \
		dune_test/moc_controller_c.cpp dune_test/program.exe

rundemo:
	PATH=`pwd`/_build/default/ppx/ppxext:$(PATH) \
	dune exec -- dune_test/program.exe

clean:
	$(RM) -r _build

uninstall:
	dune build @install
	dune uninstall

install:
	dune build @install
	dune install
