import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400; height: 300;
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
        anchors {
            left: parent.left
            right: parent.right
            bottom: parent.bottom
            top: rect1.bottom
        }
        color: "lightblue"
        height: 300
        Text {
            anchors.centerIn: parent
            text: "state = " + controller.state + "\n count = " + controller.x
        }
    }
}
