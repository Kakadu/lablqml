import QtQuick 2.1
import QtQuick.Controls 1.0

import Lablqml 1.0
/* Demonstration of direct property binding
OCaml binds to objects named "test" and "mirror".
When test changes, the binding function is called and the
mirror's text value is set */

ApplicationWindow {
    visible: true
    OCamlObject {
	id: t
	objectName: "test"
	mlvalue: "Hello world! Hello Qml with OCaml!"

	// QtObjects can only be nested as properties
	property OCamlObject nested: OCamlObject {
	   mlvalue: "I'm nested"
	}
    }

    Column {
	objectName: "mirror"
	// Starts as "Hello world ..." but when t.mlvalue changes
	Text { text: t.mlvalue } 
	// Starts as "mirror" and gets changed by `mirror_binding`
	OCamlObject { objectName: "mirror"; mlvalue: "mirror" }
    }

    // Change "test" object's `mlvalue` value
    Timer {
	interval: 500; running: true; repeat: true
	onTriggered: t.mlvalue = "Message changed"
    }

    // Terminate program after 5s
    Timer {
	interval: 5000; running: true; repeat: false
	onTriggered: Qt.quit();
    }
}
