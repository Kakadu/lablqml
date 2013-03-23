import QtQuick 2.0

Rectangle {
    id: root
    width: 800
    height: 600
    property string backgroundColor: "#FFFFDF"
    color: backgroundColor
    ListView {
        model: myModel

        width: parent.width
        orientation: ListView.Horizontal

        delegate: Rectangle {
            width: 250
            color: "lightgray"
            anchors.rightMargin: 5
            anchors.leftMargin: 5
            ListView {
                id: lv1
                currentIndex: -1
                ScrollBar {
                    flickable: lv1
                    vertical: true
                    hideScrollBarsWhenStopped: false
                    scrollbarWidth: 5
                }

                model: homm
                spacing: 5
                width: parent.width
                height: 250
                orientation: ListView.Vertical
                delegate: Rectangle {
                    radius: 5
                    anchors.rightMargin: 15
                    anchors.leftMargin: 5
                    height: 35
                    width: lv1.width - 15
                    x: 5
                    color: if (index==lv1.currentIndex) "lightgray"
                           else backgroundColor
                    Text {
                        text: qwe.name() + " " + qwe.sort()
                        anchors.fill: parent
                        font.family: "Consolas"
                        font.pixelSize: 30
                        MouseArea {
                            anchors.fill: parent
                            onClicked: lv1.currentIndex = index;
                        }
                    }
                }
            }
        }

    }
}
