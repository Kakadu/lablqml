import QtQuick 2.0

Rectangle {
    color: "green"

    ListView {
        width: 200
        height: 250

        model: myModel
        delegate: Text {
            text: "Animal: " + model.homm
            //Component.onCompleted: console.log("text completed, " + homm + ", " + model.homm);
        }
        Component.onCompleted: {
            console.log("roleNames = " + model.roles() );
            console.log("ListView completed, " + model);
        }

    }
}
