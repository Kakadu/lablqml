import QtQuick 2.1
import QtQuick.Controls 1.0

ApplicationWindow {
    QtObject {
	id: t
	objectName: "test"
	property string msg: "Hello world! Hello Qml with OCaml!"
    }
    Timer {
        interval: 500; running: true; repeat: true
        onTriggered: t.msg = "Message changed"
    }
}
