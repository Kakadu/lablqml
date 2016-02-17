import QtQuick 2.5
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.2

ApplicationWindow {
    width: 800;
    height: 600;


    Rectangle {
        id: root
        color: "#FFFFDF"
        anchors.fill: parent
        focus: true
        Keys.onEscapePressed: Qt.quit()
        Keys.onPressed: {
          if ((event.key == Qt.Key_Q) && (event.modifiers & Qt.ControlModifier))
            Qt.quit();
        }


        RowLayout {
          id: layout
          anchors.fill: parent
          spacing: 6

          Rectangle {
            color: 'teal'
            Layout.fillWidth: true
            Layout.minimumWidth: 50
            Layout.preferredWidth: 100
            Layout.maximumWidth: 300
            Layout.minimumHeight: 150
            Text {
              anchors.centerIn: parent
              text: propMap1.title
            }
          }
          Rectangle {
            color: 'plum'
            Layout.fillWidth: true
            Layout.minimumWidth: 100
            Layout.preferredWidth: 200
            Layout.preferredHeight: 100
            Text {
              anchors.centerIn: parent
              text: propMap1.count + "**2 = " + Math.pow(propMap1.count, 2)
            }
          }
        }
    }
}
