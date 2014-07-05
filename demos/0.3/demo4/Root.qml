import QtQuick 2.0
import QtQuick.Controls 1.0

ApplicationWindow {
    property string backgroundColor: "#FFFFDF"

    color: backgroundColor
    width: 400 + controller.x
    height: 600 + controller.y

    Component.onCompleted: {
      var o = controller.getobj();
      console.log(o);
      console.log(o.getname());
      console.log(o.getage() );

      controller.setobj(o);
      controller.setobj("asdf");
      controller.setobj(15);

      var p = controller.person;
      console.log(p);
      console.log(p.getname())
      console.log(p.getage());
    }
}
