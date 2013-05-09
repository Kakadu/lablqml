I have started developing from QtGui but last time I deal with QtQuick because 
it is more perspective technology.

You can execute `./configure` to get Makefile to build all QML-related things.
You can find some explanations what will happen if you execute it below.

###### Generator
Interfacing OCaml with QtQuick is performed via C++ compatibility layer. There
is a generator which parses input file in JSON format and generates C++ 
classes. Its sources are located in `../src`. Generator does not depend on any 
C++ libraries, only OCaml ones: core, ocamlgraph, yojson. To build it will be 
enough to execute 
`cd ../src && ./configure && ocamlbuild mocml/mocml.native`

######Helper library
In `lib` you can find a library for exposing OCaml-created object to QtQuick 
runtime. It does not have any special OCaml dependencies. Just don't forget to 
set correct `LD_LIBRARY_PATH` and `PKG_CONFIG_PATH` environment variables
before building.

######Test example
Test example can be found in `test`. There is Makefile there to build it. 
This example uses custom component Qt.labs.FolderListModel which can be built 
using `build_folderlistmodel` script (it will be called by generated Makefile 
too). You can built example itself using 
Makefile. Script `./configure` generates some files using `mocml` generator 
mentioned before. Don't forget to add symlink to it if it doesn't exists: 
  `ln -s ../../src/mocml.native mocml`.

N.B. This example is OCamlBrowser clone and it depends on `compiler-libs` 
ocamlfind package. Thats why you need ocaml(>=4.00.1).

After that you can run `make` and get `./main` executable. I usually test it by 
running 

    ./main -I `ocamlc -where` -I `ocamlfind query core`


