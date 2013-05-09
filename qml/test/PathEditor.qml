import QtQuick 2.0
import Qt.labs.folderlistmodel 1.0
//import org.nemomobile.folderlistmodel 1.0
import "main.js" as Main

Rectangle {
    id: dialog

    color: "#FFFFDF"
    Text {
        id: currentPathContainer
        anchors.bottomMargin: 10
        height: 25
        font.pixelSize: 20
        text: folderModel.folder
        renderType: Text.NativeRendering
    }

    property var pathModel: ["a", "b", "c", "d"]
    function removePath(index) {
        if ((0<=index) && (index<pathModel.length)) {
            for (var j=index; j<pathModel.length-1; ++j)
                pathModel[j] = pathModel[j+1];
            pathModel.pop();
            pathModelChanged();
        } else {
            console.debug("Wrong argument " + index + " in function removePath()")
        }
    }
    function addPath(s) {
        console.log (typeof(s));
        if (s.startsWith("file://"))
            s = s.substr(7);
        pathModel.push(s);
        pathModelChanged();
    }

    Row {
        height: 600
        anchors {
            left: dialog.left
            bottom: dialog.bottom
            right: dialog.right
            top: currentPathContainer.bottom
            bottomMargin: 5
            topMargin: 5
        }
        spacing: 5
        ListView {
            id: folderView
            width: 700
            height: parent.height
            clip: true
            ScrollBar {
                flickable: parent
                vertical: true
                hideScrollBarsWhenStopped: false
                scrollbarWidth: 5
                color: "green"
            }

            FolderListModel {
                id: folderModel
                folder: "/home/kakadu/.opam/4.00.1/lib"
                showDotAndDotDot: true
                showDirsFirst: true
                showHidden: true
                showFiles: false
            }

            Component {
                id: fileDelegate
                Rectangle {
                    color: dialog.color
                    width: parent.width
                    height: 30
                    MouseArea {
                        anchors.fill: parent
                        hoverEnabled: true
                        onEntered: addButton.visible = true
                        onExited: addButton.visible = false
                    }
                    Text {
                        id: nameContainer
                        font.pixelSize: 20
                        font.bold: fileIsDir
                        color: "black"
                        font.family: "Monospace"

                        text: fileName
                        height: parent.height - 2
                        anchors.left: parent.left

                        MouseArea {
                            anchors.fill: parent
                            propagateComposedEvents: true
                            onClicked: {
                                if (fileName == "..")
                                    folderModel.folder = folderModel.parentFolder;
                                else if (fileIsDir) {
                                    folderModel.folder += "/" + fileName;
                                    console.log("Change dir: " + folderModel.folder);
                                }
                                else
                                    console.log ("clicked in file " + fileName);
                            }
                        }

                    }
                    Image {
                        id: addButton
                        anchors.right: parent.right
                        anchors.rightMargin: 10
                        visible: false
                        source: "pics/plus-sign.png"
                        MouseArea {
                            anchors.fill: parent
                            propagateComposedEvents: true
                            onClicked: dialog.addPath(folderModel.folder +"/"+ fileName);
                        }
                    }
                }
            }
            model: folderModel
            delegate: fileDelegate
        }

        ListView {
            id: selectedPathsView
            width: dialog.width - parent.spacing - folderView.width
            height: parent.height

            model: pathModel
            delegate: Item {
                height: 30;
                width: parent.width

                Image {
                    anchors.left: parent.left
                    source: "pics/minus-sign.png"
                    id: deleteButton
                    MouseArea {
                        anchors.fill: parent
                        onClicked: removePath(index)
                    }
                }
                Text {
                    text: modelData
                    font.pixelSize: 20
                    anchors.left: deleteButton.right
                }
            }
        }
    }
}
