import QtQuick 2.7
import QtQuick.Controls 1.4

ApplicationWindow {
  width: 400
  height: 300

  Flow {
    property int count: beta.countB
    onCountChanged: alpha.countA = beta.countB
    Text { text: "Alpha count: "+ alpha.countA; font.pixelSize: 20 }
    Text { text: "Beta count: " + beta.countB ; font.pixelSize: 20 }
    Timer {
        interval: 3; running: true; repeat: true
        onTriggered: alpha.c0 = beta.countB
    }
    Timer {
        interval: 7; running: true; repeat: true
        onTriggered: alpha.c0 = beta.countB
    }

  }

}
