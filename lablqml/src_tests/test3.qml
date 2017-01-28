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

	// QtObjects can only be nested as properties
	property QtObject nested: QtObject {
	    property string nested_msg: "I'm nested"
	}
    }

    Column {
	objectName: "mirror"
	// Starts as "Hello world ..." but when t.msg changes
	Text { text: t.msg } 
	// Starts as "mirror" and gets changed by `mirror_binding`
	Text { objectName: "mirror"; text: "mirror" }
    }

    // Change "test" object's `msg` value
    Timer {
	interval: 50; running: true; repeat: true
	onTriggered: t.msg = "Message changed"
    }

    // Terminate program after 5s
    Timer {
	interval: 5000;
	running: true;
	repeat: false
	onTriggered: {
	    Qt.quit();
	}
    }
}
