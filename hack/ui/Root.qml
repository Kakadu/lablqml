import QtQuick 2.12
import QtQuick.Controls 2.12
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

        Label {
          anchors.top: text1.bottom
          anchors.left: text1.left
          text: controller.descr
          font.pixelSize: 22
        }
    }
    Component.onCompleted: console.log(Singleton1.someProperty)
    Button {
        text: "Click me"
        onClicked: Singleton1.doSomething()
    }
}
