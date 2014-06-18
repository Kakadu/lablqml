This is test application for OCaml and QML.

It uses PPX extension to generate some C++ code. You can build this extensions using `make -C ../../../src` command. After that you should add symlink: `ln -s ../../../src/ppx_qt.native ../ppx_qt` if it djesn't exists. Files will be gerated after executing `./configure`.


After that you can run `make` and get `./main` executable. I usually test it by running 

    ./main -I `ocamlc -where` -I `ocamlfind query compiler-libs`

Improved version of this app can be find (there)[https://github.com/Kakadu/QOcamlBrowser_quick/]

TODO: Fix property binding loop in `ApiBrowser.qml`.

