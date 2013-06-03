import QtQuick 2.0
Rectangle {
    color: "#FFFFDF" // we declare rectangle with custom background color
    width:  400;
    height: 300;     // with custom size
    Text {
        anchors.centerIn: parent
        text: "Click me!"         // text in center of window
    }
    MouseArea {
        anchors.fill: parent      // all space in parent Rectangle will be clickable
        onClicked: console.log("clicked")
    }
}