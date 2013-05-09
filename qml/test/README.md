This is test application for OCaml and QML.

It uses `./mocml` generator to generate some code. You can build it using `make -C ../../../src` command. After that you should add symlink: `ln -s ../../../src/mocml.native mocml`. Files will be gerated after executing `./configure`.

This app depends on Qt5. It is not installed on computer system-widely. I have built it from Git and created environment variable `$QT5_ROOT`. So to build this app you need to export this app. `qmake` for Qt5 is usually located in `$QT5_ROOT/qtbase/bin`.

It uses patched FolderListModel from Qt Labs. Use `./build_folderlistmodel` to apply patch and build it.

After that you can run `make` and get `./main` executable. I usually test it by running 

    ./make -I `ocamlc -where` -I `ocamlfind query core`


