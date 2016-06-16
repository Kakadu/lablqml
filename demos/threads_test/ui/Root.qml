import QtQuick 2.1
import QtQuick.Controls 1.0

ApplicationWindow {
    property string backgroundColor: "#FFFFDF"

    Rectangle {
        id: root
        color: backgroundColor
        width: 800; height: 600;
        anchors.fill: parent
        focus: true
        Keys.onEscapePressed: Qt.quit()
        Keys.onPressed: {
          if ((event.key == Qt.Key_Q) && (event.modifiers & Qt.ControlModifier))
            Qt.quit();
        }

        Text {
          anchors.centerIn: parent
          text: controller.descr
        }
    }
}
