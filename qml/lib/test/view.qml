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

    Flickable {
        id: flick
        flickableDirection: Flickable.VerticalFlick
        //contentHeight: descriptionTextField.painterHeight
        //contentWidth:  descriptionTextField.paintedWidth
        //width: 300; height: 200;
        x: 0; y: mainView.height
        /*
        //anchors.bottom: root.bottom
        function ensureVisible(r)
        {
            if (contentX >= r.x)
            contentX = r.x;
            else if (contentX+width <= r.x+r.width)
            contentX = r.x+r.width-width;
            if (contentY >= r.y)
            contentY = r.y;
            else if (contentY+height <= r.y+r.height)
            contentY = r.y+r.height-height;
        }*/

        TextEdit {
            id: descriptionTextField
            width: flick.width
            height: flick.height
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
            //onCursorRectangleChanged: flick.ensureVisible(cursorRectangle)
        }
        Component.onCompleted: console.log(contentHeight + ", " + contentWidth);
    }

}
