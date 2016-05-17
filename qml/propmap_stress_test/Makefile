all:
	ocamlfind opt -package lablqml -ppx "ppx_qt -destdir . -ext cpp" -thread -linkpkg \
	-cclib -lstdc++ -ccopt -L`qmake -query QT_INSTALL_LIBS` \
	-cclib -lQt5Quick -cclib -lQt5Qml -cclib -lQt5Network \
	-cclib -lQt5Widgets -cclib -lQt5Gui -cclib -lQt5Core \
	-g main.ml -o test.native
