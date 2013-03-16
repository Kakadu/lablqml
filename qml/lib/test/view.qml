import QtQuick 2.0

Rectangle {
    id: root
    width: 800
    height: 600
    color: "white"
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
                delegate:
                    Rectangle {
                        radius: 5
                        anchors.rightMargin: 15
                        anchors.leftMargin: 5
                        width: lv1.width - 15
                        x: 5
                        height: 35
                        color: "black"
                        Text { text: qwe.name() + " " + qwe.sort()
                            anchors.fill: parent
                            color: "white"
                            font {
                                family: "Consolas"
                                pixelSize: 30
                            }
                        }
                }
            }
        }

    }
}
