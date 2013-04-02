import QtQuick 2.0
import Qt.labs.folderlistmodel 1.0

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
    ListModel {
        id: pathModel
        ListElement { name: "a" }
        ListElement { name: "b" }
    }

    property variant pathModel: ["a", "b"]

    Row {
        height: 600
        anchors {
            left: dialog.left
            bottom: dialog.bottom
            right: dialog.right
            top: currentPathContainer.bottom
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
                folder: "/home/kakadu/mand"
                showDotAndDotDot: true
                showDirsFirst: true
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
                        font.family: if (fileIsDir) "Terminal"
                                     else "Consolas"

                        text: fileName
                        height: parent.height - 2
                        anchors.left: parent.left

                        MouseArea {
                            anchors.fill: parent
                            propagateComposedEvents: true
                            onClicked: {
                                //console.log(folderModel.folder);
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
                            onClicked: {
                                //var o = Qt.createQmlObject("import QtQuick 2.0; ListElement { name: \"" + fileName + "\" }", pathModel, "");
                                //pathModel.append(o);
                                // does not implemented yet
                                console.log('addButton clicked: ' + fileName);
                                console.log(pathModel)
                            }
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
                        onClicked: pathModel.remove(index)
                    }
                }
                Text {
                    text: name
                    font.pixelSize: 20
                    anchors.left: deleteButton.right
                }
            }
        }
    }
}
