This Demo shows how to declare properties and signals on OCaml side.

To build it you need lablqt >=0.4, OCaml>=4.02 and Qt>=5.3.

When user clicks on MouseArea QML sends to OCaml that user have clicked. OCaml increments
click counter and emits signal to QML that property have changed. Also OCaml emits signal
with custom message. This message will be printed in QML side to console.

===================

N.B. This example is not built automatically by `$LABLQT_ROOT/qml/configure`.

N.B. Don't forget that QtQuick 1.0 == QtDeclarative from Qt 4.x,
QtQuick 2.x == QtDeclarative from Qt5.

