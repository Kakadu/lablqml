[![Build Status](https://travis-ci.org/Kakadu/lablqml.svg?branch=master)](https://travis-ci.org/Kakadu/lablqml)

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

### Installed files

```
$ opam show lablqml --list-files
/home/kakadu/.opam/4.07.0+fp+flambda/bin/ppx_qt
/home/kakadu/.opam/4.07.0+fp+flambda/doc/lablqml/CHANGELOG
/home/kakadu/.opam/4.07.0+fp+flambda/doc/lablqml/README.md
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/META
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/dune-package
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.a
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cma
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmi
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmt
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmti
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmx
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmxa
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cmxs
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.h
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.ml
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.mli
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/liblablqml_stubs.a
/home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/opam
/home/kakadu/.opam/4.07.0+fp+flambda/lib/stublibs/dlllablqml_stubs.so
```

There is a library here. It's fine to use it for simple programs

There is also `ppx_qt` executable. It is used to generate C++ code from OCaml. 
See `demos/` for examples

### Minimal ocamlfind-based helloworld

    ➜  cat a.ml                                                                                                                     
```
open Lablqml

let main () = ()
let () =
  run_with_QQmlApplicationEngine Sys.argv main "Root.qml"
```

    ➜  cat Root.qml                                                                                   
```
import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    color: "#FFFFDF" // we declare rectangle with custom background color
    width:  400;
    height: 300;     // with custom size
    Text {
        anchors.centerIn: parent
        text: "Click me!"
    }
    MouseArea {
        anchors.fill: parent      // all space in parent Rectangle will be clickable
    }
}
```

    ➜  ocamlfind opt -package lablqml a.ml -thread -linkpkg -cclib -lQt5Quick -cclib -lQt5Qml -cclib -lQt5Network -cclib -lQt5Widgets -cclib -lQt5Gui -cclib -lQt5Core -cclib -lstdc++
    ➜  ./a.out

### Playing with REPL

    # #thread;;
    /home/kakadu/.opam/4.07.0+fp+flambda/lib/ocaml/threads: added to search path
    /home/kakadu/.opam/4.07.0+fp+flambda/lib/ocaml/unix.cma: loaded
    /home/kakadu/.opam/4.07.0+fp+flambda/lib/ocaml/threads/threads.cma: loaded
    # #require "lablqml";;
    /home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml: added to search path
    /home/kakadu/.opam/4.07.0+fp+flambda/lib/lablqml/lablqml.cma: loaded

