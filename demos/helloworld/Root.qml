import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    color: "#FFFFDF" // we declare rectangle with custom background color
    width:  400;
    height: 300;     // with custom size
    Text {
        anchors.centerIn: parent
        text: "Click me!"
    }
    MouseArea {
        anchors.fill: parent      // all space in parent Rectangle will be clickable
        onClicked: controller.onMouseClicked("message")
    }
}
