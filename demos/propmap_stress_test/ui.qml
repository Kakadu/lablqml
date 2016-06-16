import QtQuick 2.5
import QtQuick.Controls 1.4

ApplicationWindow {
  width: 400
  height: 300

  Flow {
    property int count: beta.count
    onCountChanged: alpha.count = beta.count
    Text { text: "Alpha count: "+ alpha.count; font.pixelSize: 20 }
    Text { text: "Beta count: " + beta.count ; font.pixelSize: 20 }
    Button { text: "Fire"; onClicked: alpha.count2 = beta.count + 1 }
    Timer {
        interval: 3; running: true; repeat: true
        onTriggered: alpha.c0 = beta.count
    }
    Timer {
        interval: 7; running: true; repeat: true
        onTriggered: alpha.c0 = beta.count
    }

  }

}
