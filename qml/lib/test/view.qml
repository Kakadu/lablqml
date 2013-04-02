import QtQuick 2.0
import Qt.labs.folderlistmodel 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 19
    property int defaultTextFieldHeight: defaultFontSize + 4

    id: root
    color: backgroundColor
    Rectangle {
        id: browseAPIContainer
        //color: backgroundColor
        anchors {
            top: parent.top
            left: parent.left
            right: parent.right
            bottom: bottomToolbar.top
        }

        ListView {
            id: mainView
            model: myModel
            height: 400
            width: parent.width
            orientation: ListView.Horizontal
            spacing: 5

            ScrollBar {
                flickable: parent
                vertical: false
                hideScrollBarsWhenStopped: false
                scrollbarWidth: 5
                color: "red"
            }
            delegate: Rectangle {
                property int mainIndex: index
                height: 400
                width: (mainView.width - (mainView.count-1)*mainView.spacing) / mainView.count
                color: backgroundColor

                anchors.rightMargin: 5
                anchors.leftMargin: 10

                ListView {
                    id: lv1
                    width: parent.width
                    height: parent.height - 10

                    currentIndex: -1
                    ScrollBar {
                        flickable: lv1
                        vertical: true
                        hideScrollBarsWhenStopped: false
                        scrollbarWidth: 5
                    }
                    highlight: Rectangle {
                        color: backgroundColor
                        radius: 3; opacity: 0.7
                        anchors.leftMargin: 5
                        anchors.rightMargin: 15
                        width: parent.width - 10
                        border.width: 1
                        //y: lv1.currentItem.y
                        //Behavior on y { SpringAnimation { spring: 3; damping: 0.2 } }
                    }
                    //highlightMoveDuration: -1
                    clip: true
                    model: homm
                    spacing: 5
                    orientation: ListView.Vertical

                    delegate: Text {
                        anchors.rightMargin: 15
                        anchors.leftMargin: 5
                        height: defaultTextFieldHeight
                        width: lv1.width - 15
                        x: 5
                        text: qwe.name() + " (" + qwe.sort() + ")"
                        font.family: "Consolas"
                        font.pixelSize: defaultFontSize
                        MouseArea {
                            anchors.fill: parent
                            onClicked: {
                                lv1.currentIndex = index;
                                controller.onItemSelected(mainIndex,index);
                            }
                        }
                    }
                }
            }
        }

        Scrollable {
            flickableDirection: Flickable.VerticalFlick
            contentHeight: descriptionTextField.height
            contentWidth:  descriptionTextField.width
            color: backgroundColor
            anchors.top: mainView.bottom
            anchors.left: mainView.left
            anchors.right: mainView.right
            anchors.bottom: browseAPIContainer.bottom
            hideScrollBarsWhenStopped: true

            TextEdit {
                id: descriptionTextField

                font.family: "Monospace"
                font.pixelSize: defaultFontSize
                focus: true
                selectByMouse: true
                readOnly: true
                textFormat: TextEdit.RichText

                text: {
                    if (controller.hasData) controller.descr
                    else "No description here"
                }
            }
        }
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
            PropertyChanges { target: bottomToolbar; text: qsTr("Edit search paths") }
        },
        State {
            name: "EDIT_PATHS"
            PropertyChanges { target: editPathsContainer; visible: true }
            PropertyChanges { target: browseAPIContainer; visible: false }
            PropertyChanges { target: bottomToolbar; text: qsTr("Back to API browsing") }
        }
    ]
    state: "BROWSE_API"
    Text {
        id: bottomToolbar
        anchors.left: root.left
        anchors.right: root.right
        anchors.bottom: root.bottom
        font.pixelSize: defaultFontSize
        font.family: "Monospace"
        height: defaultTextFieldHeight
        color: "brown"
        MouseArea {
            anchors.fill: parent
            onClicked: {
                switch (root.state) {
                case "BROWSE_API": root.state = "EDIT_PATHS"; break;
                case "EDIT_PATHS": root.state = "BROWSE_API"; break;
                default:
                    console.error("Error while switching state")
                }
            }
        }
    }
}
