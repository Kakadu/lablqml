import QtQuick 2.1
import QtQuick.Controls 1.2
import mycomponents 1.0

ApplicationWindow {
    title: "User signal and property demo"
    property string backgroundColor: "#FFFFDF"

    Connections {
        target: controller
        onHiGotten: console.log(message)
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
        color: "#b2ebbe"
        height: 150
        Text {
            text: "I will count how many times you clicked"
            anchors.centerIn: parent
        }
    }
    Rectangle {
        anchors {
           left: parent.left
           right: parent.right
           bottom: parent.bottom
           top: rect2.bottom
        }
        color: "#ebf097"
        height: 150
        Text {
            text: controller.clicksCount
            anchors.centerIn: parent
        }
    }
}
