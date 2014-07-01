import QtQuick 2.0
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

ApplicationWindow {
    title: "OCaml&QtQuick demo 2"
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400
    height: 450

    ColumnLayout {
      Rectangle {
        color: "lightgreen"
        Layout.preferredHeight: 150
        Layout.preferredWidth: 400
        Text {
          text: "Click me!"
          anchors.centerIn: parent
        }
        MouseArea {
          anchors.fill: parent
          onClicked: controller.onMouseClicked()
        }
      }
      Rectangle {
        color: "#ebf097"
        Layout.preferredHeight: 150
        Layout.preferredWidth:  400
        Text {
          text: "You have clicked " + controller.clicksCount + " times"
          anchors.centerIn: parent
        }
      }
    }
}
