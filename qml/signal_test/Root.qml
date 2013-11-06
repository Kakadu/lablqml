import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"

    Connections {
        target: controller
        onEbanashka: console.log(message)
    }
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
            onClicked: controller.onMouseClicked()
        }
    }

    Rectangle {
        id: rect2
        anchors {
            left: parent.left
            right: parent.right
            top: rect1.bottom
        }
        color: "lightblue"
        height: 150
    }
    Rectangle {
        anchors {
           left: parent.left
           right: parent.right
           bottom: parent.bottom
           top: rect2.bottom
        }
        color: "yellow"
        height: 150
    }
}
