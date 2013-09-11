import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

SplitView {
    id: browseAPIContainer
    orientation: Qt.Vertical

    ListView {
        id: mainView
        model: myModel
        width: parent.width

        orientation: ListView.Horizontal
        spacing: 5
        // SplitView props
        Layout.minimumHeight: 100
        Layout.maximumHeight: 600
        Layout.fillHeight: true

        ScrollBar {
            flickable: parent
            vertical: false
            hideScrollBarsWhenStopped: false
            scrollbarWidth: 5
            color: "red"
        }
        delegate: Rectangle {
            property int mainIndex: index
            height: parent.height
            width: (mainView.width - (mainView.count-1)*mainView.spacing) / mainView.count
            color: backgroundColor

            anchors.rightMargin: 5
            anchors.leftMargin: 10
            ScrollView {
                width: parent.width
                height: parent.height - 5
                contentItem: ListView {
                    id: lv1
                    anchors.fill: parent
                    currentIndex: -1

                    highlight: Rectangle {
                        color: backgroundColor
                        radius: 3
                        opacity: 0.7
                        anchors.leftMargin: 5
                        anchors.rightMargin: 15
                        width: parent.width - 10
                        border.width: 1
                    }
                    highlightMoveDuration: 300
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
                                currentPathHolder.text = controller.getFullPath()
                            }
                        }
                    }
                }
            }
        }
    }

    Text {
        id: currentPathHolder
        height: defaultTextFieldHeight
        Layout.minimumHeight: defaultTextFieldHeight
        Layout.maximumHeight: defaultTextFieldHeight
        font.pixelSize: defaultFontSize
        font.bold: true
        anchors {
            left: mainView.left
            right: mainView.right
        }
        text: ""
    }

    ScrollView {
        id: descriptionTextFieldHolder
        anchors.left: currentPathHolder.left
        anchors.right: currentPathHolder.right
        Layout.minimumHeight: 100
        Layout.maximumHeight: 400
        height: 300

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
