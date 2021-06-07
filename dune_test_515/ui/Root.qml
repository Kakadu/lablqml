import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Layouts 1
import com.mycompany.qmlcomponents 1.0

ApplicationWindow {
  property string backgroundColor: "#FFFFDF"
  width: 800;
  height: 600;

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

    Label {
      id: text1
      anchors.centerIn: parent
      text: Singleton1.someProperty
      font.pixelSize: 22
    }
  }
  Component.onCompleted: {
    console.log("accessing a property: ", Singleton1.someProperty)
    console.log("accessing an custom object", myslider1)
  }

  ColumnLayout {
    spacing: 2

    Button {
        Layout.alignment: Qt.AlignCenter
        text: "Action 1"
        onClicked: Singleton1.action1()
    }

    Button {
        Layout.alignment: Qt.AlignCenter
        text: "Action 2"
        onClicked: Singleton1.action2()
    }
  }
}
