import QtQuick 2.0

Rectangle {
    property string backgroundColor: "#FFFFDF"
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 20
    property int defaultTextFieldHeight: 25

    id: root
    color: backgroundColor

    ListView {
        id: mainView
        model: myModel
        height: 400
        width: parent.width
        orientation: ListView.Horizontal
        spacing: 10

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
            width: mainView.width / mainView.count - 5
            color: backgroundColor

            anchors.rightMargin: 5
            anchors.leftMargin: 10
            Component.onCompleted: console.log("Main delegate.width = " + width);
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
                    //Component.onCompleted: console.log("hightlight.width = " + width);
                }
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
                    text: qwe.name() + " " + qwe.sort()
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
    Rectangle {
      Text {
        id: descriptionTextField
        x: 0; y: mainView.height
        font.family: "Monospace"
        font.pixelSize: defaultFontSize
        text: {
            if (controller.hasData) controller.descr
            else "<No description here>"
        }
      }
    }

}
