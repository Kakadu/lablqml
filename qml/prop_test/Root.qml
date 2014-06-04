import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400 + controller.x
    height: 600 + controller.y
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
        Text {
            anchors.centerIn: parent
            text: "state = " + controller.state + "\n count = " + controller.x
        }
    }
    Rectangle {
        anchors {
           left: parent.left
           right: parent.right
           bottom: parent.bottom
           top: rect2.bottom
        }
        height: 300
        ListView {
            anchors.fill: parent
            orientation: ListView.Vertical
            model: intModel
            delegate:
                Text {
                    width: 50
                    height: 20
                    text: title + " " + obj.text
                    //text: obj.text
                    Component.onCompleted: console.log("text field created with text " + cellX)
                }

        }
    }
}
