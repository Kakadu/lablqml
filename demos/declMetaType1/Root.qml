import QtQuick 2.0
import QtQuick.Controls 1.0
import com.asdf 1.0

ApplicationWindow {
    width:  400;
    height: 300;     // with custom size

    function somePropertyChanged(newval) {
      console.log("received: " + newval);
    }

    DemoClass1 {
      id: d1
      Component.onCompleted: countChanged.connect(somePropertyChanged);
    }
    Text {
      anchors.centerIn: parent
      text: "We should call OCaml every second in the background and print on console"
    }
    Timer {
      running: true
      repeat: true
      interval: 1000
      onTriggered: {
        d1.tick();
        console.log("return from ocaml: " + d1.getcount() );
      }
    }
}
