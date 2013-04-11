I have started developing from QtGui but last time I deal with QtQuick because it is more perspective technology.

In this repo you can find generator for interfacing OCaml and QML. Its sources are located in `../src`. Generator does not depend on any C++ libraries, only OCaml ones: core, ocamlgraph, yojson.

To build generator it will be enough to execute 
`cd ../src && ./configure && ocamlbuild mocml/mocml.native`


Test example uses library which can be found at `lib`. Do not forget to configure your pkg-config for Qt5.

Test example can be found in `lib/test`. There is Makefile there to build it. This example uses custom component Qt.labs.FolderListModel which can be built using `build_forlderlistmodel` script. You can built example itself using Makefile. Script `./configure` generates some files using `mocml` generator mentioned before. Dont forget to add symlink to it: `ln -s ../../../src/mocml.native mocml`.

N.B. This example is OCamlBrowser clone and it depends on `compiler-libs` ocamlfind package. Thats why you need ocaml(>=4.00.1).


After that you can run `make` and get `./main` executable. I usually test it by running 

    ./main -I `ocamlc -where` -I `ocamlfind query core`


