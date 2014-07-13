import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    title: "Hello world!"
    color: "#FFFFDF"
    width: 400
    height: 450

    Rectangle {
        color:  "lightgreen"
        height: 150
        anchors {
            left:  parent.left
            right: parent.right
            top:   parent.top
        }
        Text {
            text: "Click me!"
            anchors.centerIn: parent
        }
        MouseArea {
	    anchors.fill: parent
            onClicked: {
              // call OCaml there
              controller.onMouseClicked()
            }
	}
    }
}

