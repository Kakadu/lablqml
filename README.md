In this repo you can find my work about integrating OCaml and Qt. Initially. I 
was planning to generate binding code for OCaml and Qt Widgets but I found it 
hard to implement mostly because Qt relies on onheritance very much (we can't 
add onClick handler for Button without inheritance). Later I decided to 
generate binding code for OCaml and [QtQuick](http://www.qt.io/qt-quick/).

So this repository should be called as lablqtquick or lablqml but I keep 
`lablqt` for historical reasons.

Directory mapping:

* Code generator for QtQuick 2.0 is in `src`.
* Library for interfacing with QtQuick 2 is in `lablqml`.
* Test example is now in `qml/test`
* Demo app `qocamlbrowser` (ocamlbrowser with QtQuick instead of Tcl/Tk) is 
located in [separate repo](https://github.com/kakadu/qocamlbrowser).
 
Github pages [site](http://kakadu.github.io/lablqt/) and 
[tutorial](http://kakadu.github.io/lablqt/tutorial2.html) are available.

Use `./configure && make` to build it. Don't forget to install good Qt 
version (>= 5.3 I think).

Waiting for your feedback,
Kakadu

