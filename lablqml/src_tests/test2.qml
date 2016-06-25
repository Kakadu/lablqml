import QtQuick 2.1
import QtQuick.Controls 1.0

ApplicationWindow {

  Component.onCompleted: {
      runner.run();
      Qt.quit();
  }
}
