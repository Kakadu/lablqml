import QtQuick 2.1
import QtQuick.Controls 1.0

/* Demonstraction of direct property binding
OCaml binds to objects named "test" and "mirror".
When test changes, the binding function is called and the
mirror's text value is set */

ApplicationWindow {
    visible: true
    QtObject {
	id: t
	objectName: "test"
	property string msg: "Hello world! Hello Qml with OCaml!"
	property QtObject nested: QtObject {
	    property string nested_msg: "I'm nested"
	}
    }

    Column {
	objectName: "mirror"
	Text { text: t.msg }
	Text { objectName: "mirror"; text: "mirror" }
    }

    Timer {
        interval: 50; running: true; repeat: true
        onTriggered: t.msg = "Message changed"
    }

    Timer {
         interval: 5000;
         running: true;
         repeat: false
         onTriggered: {
             Qt.quit();
         }
    }
}
