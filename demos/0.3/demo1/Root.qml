import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    title: "OCaml&QtQuick demo 1"
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400
    height: 450
    Rectangle {
        id: rect1
        color: "lightgreen"
        anchors {
            left: parent.left
            right: parent.right
            top: parent.top
        }
        height: 150
        Text {
            text: "Click me!"
            anchors.centerIn: parent
        }
        MouseArea {
            anchors.fill: parent
            onClicked: controller.onMouseClicked2()
        }
    }

}
