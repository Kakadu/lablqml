import QtQuick 2.1
import QtQuick.Controls 1.0

ApplicationWindow {
    property string backgroundColor: "#FFFFDF"
    width: 800; height: 600;

    Rectangle {
        id: root
        color: backgroundColor
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
