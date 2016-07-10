import QtQuick 2.1
import QtQuick.Controls 1.0

ApplicationWindow {

    Timer {
         interval: 500; 
         running: true; 
         repeat: false
         onTriggered: {
             runner.run();
             Qt.quit();
         }
    }
}
