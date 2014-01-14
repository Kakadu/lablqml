import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400
    height: 600


    MouseArea {

        anchors.fill: parent /*
        onClicked: {
            console.log("mouse clicked in QML. Calling caml");
            //controller.slot1("privet!")
        } */
        Connections {
            onClicked: controller.prop1 = false
        }

    }

    Text {
        text: "Click me!"
        anchors.centerIn: parent
    }

    Component.onCompleted: {
        console.log("Controller = " + controller);
        controller.pizda();
    }
}
