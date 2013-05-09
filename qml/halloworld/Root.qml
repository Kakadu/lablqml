import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 800; height: 600;
    Text {
        anchors.centerIn: parent
        text: "Click me!"
    }
    MouseArea {
        anchors.fill: parent
        onClicked: {
          controller.onMouseClicked()
        }
    }
}

