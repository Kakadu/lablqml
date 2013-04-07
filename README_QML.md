This repo contins my thoughts about interfacing Qt and OCaml.

I have started developing from QtGui but last time I deal with QtQuick because it is more perspective technology.

In this repo you can find generator for interfacing OCaml and QML. Its sources are located in src/. Generator does not depend on any C++ libraries, only OCaml ones: core, ocamlgraph, tarjangraph.
  * core ia available in opam
  * library tarjangraph you can find on [github](https://github.com/Kakadu/tarjan)
    TODO: find suitable algorithm on ocamlgraph and remove this dependence.
  * ocaml-syck library to deal with YAML files. Use `bzr branch lp:ubuntu/ocaml-syck` to get it.
    N.B. just `make && make install DEST=....`, no any `./configure`.
    TODO: finish my opam patches about bazaar support.


When all dependencies are installed you can build generator.

Test example uses library which can be found at `qml/lib`. It depends on `$QT5` environment variable. It should point to a valid Qt5 source tree.
TODO: Use `qmake -query {QT_INSTALL_PREFIX,QT_INSTALL_BINS,QT_INSTALL_HEADERS}` instead of hardcoded paths.

Test example can be found in `qml/lib/test`. There is Makefile there to build it. This example uses custom component Qt.labs.FolderListModel which can be built using `build_forlderlistmodel` script. You can built example itself using Makefile. Script `./configure` generates some files using `mocml` generator mentioned before. Dont forget to add symlink to it: `ln -s ../../../src/mocml.native mocml`.

N.B. This example is OCamlBrowser clone and it depends on `compiler-libs` ocamlfind package. Thats why you need ocaml(>=4.00.1).

TODO: rewrite build scripts to avoid environment variables.

After that you can run `make` and get `./main` executable. I usually test it by running 

    ./main -I `ocamlc -where` -I `ocamlfind query core`


