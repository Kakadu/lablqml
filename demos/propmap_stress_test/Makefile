.PHONY: all clean

PPX_QT=ppx_qt
OUT=test.native

ifeq ($(VERBOSE),1)
V=
else
V=@
endif


all:
	$(V)PATH=`pwd`/../../src/:$$PATH \
	OCAMLPATH=`pwd`/../../lib/_build/bundle \
	ocamlfind opt -package lablqml -ppx "$(PPX_QT) -destdir . -ext cpp" \
	-thread -linkpkg \
	-cclib -lstdc++ -ccopt -L`qmake -query QT_INSTALL_LIBS` \
	-cclib -lQt5Quick -cclib -lQt5Qml -cclib -lQt5Network \
	-cclib -lQt5Widgets -cclib -lQt5Gui -cclib -lQt5Core \
	-g main.ml -o $(OUT)

clean::
	$(RM) -f *.o $(OUT) *.cm[oix]
