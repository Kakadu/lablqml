In this repo you can find my work about integrating OCaml and Qt. Initially, I 
was planning to generate binding code for OCaml and Qt Widgets but I found it 
hard to implement mostly because Qt relies on inheritance very much (we can't 
add onClick handler for button without inheritance). Later I decided to 
generate binding code for OCaml and [QtQuick](http://www.qt.io/qt-quick/). If 
you really need QtWidgets you can check out 
[mrvn's work](https://github.com/mrvn/ocaml-qt5). But I believe that QtQuick
is the right way to create UI in Qt right now.

So this repository should be called as `lablqtquick` or `lablqml` but I kept 
`lablqt` for historical reasons.

Directories mapping:

* Code generator for QtQuick 2 is in `src`. It is made as ppx syntax extension
* Library for interfacing with QtQuick 2 is in `lablqml`.
* Demos are in `demos/`. Use `./configure && make test` to build them.
* Demo app `qocamlbrowser` (ocamlbrowser with QtQuick instead of Tcl/Tk) is 
located in [separate repo](https://github.com/kakadu/qocamlbrowser).
 
Github pages [site](http://kakadu.github.io/lablqt/) and 
[tutorial](http://kakadu.github.io/lablqt/tutorial2.html) are available.

Use `./configure && make` to build it. Don't forget to install good Qt 
version (>= 5.3 I think, `./configure` will check for it, btw).
