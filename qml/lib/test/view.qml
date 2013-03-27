import QtQuick 2.0

Rectangle {
//    width: 800
//    height: 600
    property string backgroundColor: "#FFFFDF"
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 20
    property int defaultTextFieldHeight: 25

    id: root
    color: backgroundColor

    ListView {
        model: myModel
        height: 400
        width: parent.width
        orientation: ListView.Horizontal
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
            width: 250
            color: "lightgray"
            anchors.rightMargin: 5
            anchors.leftMargin: 5
            ListView {
                id: lv1
                width: parent.width
                height: parent.height - 40

                currentIndex: -1
                ScrollBar {
                    flickable: lv1
                    vertical: true
                    hideScrollBarsWhenStopped: false
                    scrollbarWidth: 5
                }

                model: homm
                spacing: 5
                orientation: ListView.Vertical
                delegate: Rectangle {
                    radius: 5
                    anchors.rightMargin: 15
                    anchors.leftMargin: 5
                    height: defaultTextFieldHeight
                    width: lv1.width - 15
                    x: 5
                    color: if (index==lv1.currentIndex) "lightgray"
                           else backgroundColor
                    Text {
                        text: qwe.name() + " " + qwe.sort()
                        anchors.fill: parent
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

    }
}
