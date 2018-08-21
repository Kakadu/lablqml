Let's write QML/QtQuick GUI for OCaml (or Reason) using *lablqml*!

In this repo you can find my work about integrating OCaml and Qt. Initially, I
planned to generate binding code for OCaml and Qt Widgets and called this
library `lablqt`. But I found it
hard to implement mostly because Qt relies on inheritance very much (we can't
add onClick handler for button without inheritance). Later I decided to
generate binding code for OCaml and [QML/QtQuick](http://www.qt.io/qt-quick/).
And I believe that QtQuick is the right way to create UI in Qt right now.
See [online book](https://qmlbook.github.io/) for getting started with Qt/QML itself

From 14th of February 2017 it's renamed from `lablqt` to `lablqml` because it
is actually about QtQuick and not QtWidgets or Qt itself.  If
you really need QtWidgets you can check out 
[mrvn's work](https://github.com/mrvn/ocaml-qt5) or 
[def](https://github.com/let-def/cuite)'s.

Directories mapping:

* PPX syntax extension with code generator is in `ppx/`.
* Library for interfacing with QtQuick 2 is in `lib`.
* Demo that can be build during opam installation is in `dune_test`. 
  Use `make demo` to build it.
* Old demos that are not using `dune` as build system are in `demos/`.
* Demo app `qocamlbrowser` (ocamlbrowser with QtQuick instead of Tcl/Tk) is
located in [separate repo](https://github.com/kakadu/qocamlbrowser_quick).

Github pages [site](http://kakadu.github.io/lablqml/) and
[tutorial](http://kakadu.github.io/lablqml/tutorial2.html) are available.

Use `./configure && make` to build it. Don't forget to install `g++` and good Qt
version (>= 5.3 I think, `./configure` will check for it, btw).
