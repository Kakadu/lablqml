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
    Button { text: "Fire"; onClicked: alpha.count = beta.count + 1 }
  }

}
