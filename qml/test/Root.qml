import QtQuick 2.0
import Qt.labs.folderlistmodel 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 19
    property int defaultTextFieldHeight: defaultFontSize + 4

    id: root
    color: backgroundColor
    width: 800; height: 600;

    ApiBrowser {
        id: browseAPIContainer
        anchors {
            top: parent.top
            left: parent.left
            right: parent.right
            bottom: bottomToolbar.top
        }
        color: backgroundColor
    }

    PathEditor {
        id: editPathsContainer
        anchors {
            top: parent.top
            left: parent.left
            right: parent.right
            bottom: bottomToolbar.top
        }
    }

    states: [
        State {
            name: "BROWSE_API"
            PropertyChanges { target: browseAPIContainer; visible: true }
            PropertyChanges { target: editPathsContainer; visible: false }
            PropertyChanges { target: switchStateButton; text: qsTr("Edit search paths") }
        },
        State {
            name: "EDIT_PATHS"
            PropertyChanges { target: editPathsContainer; visible: true }
            PropertyChanges { target: browseAPIContainer; visible: false }
            PropertyChanges { target: switchStateButton; text: qsTr("Back to API browsing") }
        }
    ]
    state: "BROWSE_API"
    function setCurrentPaths() {
        // get OCaml paths and set them to temporary model
        var lst = controller.paths() // So hackful because we need to convert QList<String> to Array
        var ans = [];
        for (var x in lst ) ans.push(lst[x])
        editPathsContainer.pathModel = ans
    }
    function applyPaths() {
        // transfer selected paths to OCaml
        console.log("applyPaths()")
        controller.setPaths(editPathsContainer.pathModel)
    }

    Rectangle {
        id: bottomToolbar
        anchors.left: root.left
        anchors.right: root.right
        anchors.bottom: root.bottom
        anchors.topMargin: 10
        height: 35
        color: backgroundColor

        Rectangle {
            radius: 10
            height: 30
            width: 250
            color: backgroundColor
            border { color: "darkgray"; width: 3 }

            x: 0; y:0
            Text {
                id: switchStateButton
                anchors.centerIn: parent
                font.pixelSize: defaultFontSize
                font.family: "Monospace"
                height: defaultTextFieldHeight
                color: "black"

                MouseArea {
                    anchors.fill: parent
                    onClicked: {
                        switch (root.state) {
                        case "BROWSE_API":
                            setCurrentPaths();
                            console.log(editPathsContainer.pathsModel)
                            root.state = "EDIT_PATHS";
                            break;
                        case "EDIT_PATHS":
                            applyPaths();
                            root.state = "BROWSE_API";
                            break;
                        default:
                            console.error("Error while switching state")
                        }
                    }
                }
            }
        }
    }
}
