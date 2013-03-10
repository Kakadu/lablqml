import QtQuick 1.0

Rectangle {
    width:  gameMap.sizex();
    height: 600;
    Text {
        text: gameMap.title;
        anchors.centerIn: parent
    }
}
